(ql:quickload '(:parser-combinators :alexandria :dexador :jonathan))
(ql:quickload :cl-arrows)
(ql:quickload :bt-semaphore)
(ql:quickload :lparallel)


(defpackage papergraph
  (:use :cl :parser-combinators :alexandria :cl-arrows)
  (:export #:process-entries
           #:process-graph))
(in-package :papergraph)


(def-cached-parser bib-file?
  (sepby? (choice (bib-entry?) (bib-comment?))
          (white?)))


(def-cached-parser bib-comment?
  (named-seq?
   "%" (<- comment (gather-if-not* (rcurry 'member '(#\Newline)))) #\Newline
   (list :comment (concatenate 'string comment))))


;; Parse one bib-entry
(def-cached-parser bib-entry?
  (named-seq?
   ;; Parse the type
   (<- type (bib-entry-type?))
   ;; Some whitespace and an opening brace
   (white?) "{" (white?)
   ;; First comes the name of the entry, followed by a comma
   (<- name (bib-name?))
   (white?) "," (white?)
   ;; Now parse all entries
   (<- entries (many? (bib-entry-item?)))
   "}" (white?)
   (append (list (list :bib-entry-type type) (list :bib-entry-name name)) entries)))


;; Zero or more whitespace
(def-cached-parser white?
  (between? (sat (rcurry #'member '(#\Space #\Newline #\Tab)))
            nil nil 'null))


;; Parse the name of a bib-entry, i.e., up until whitespace or comma
(def-cached-parser bib-name?
  (named-seq?
   (<- name-list (gather-if-not* (rcurry 'member '(#\Space #\Newline #\Tab #\, #\} #\=))))
   (concatenate 'string name-list)))


;; Parse the type of a bib-entry, i.e., something starting with @ and then a word
(def-cached-parser bib-entry-type?
  (named-seq?
   "@"
   (<- type (word?))
   type))


;; Parse one item in a bib-entry, i.e., something with a name, a "=" and then a
;; more or less compilcated entry
(def-cached-parser bib-entry-item?
  (named-seq?
   (<- name (bib-name?))
   (white?) "=" (white?)
   (<- entry (choice (word?) (bib-entry-value?)))
   (white?) (opt? ",") (white?)
   (list (intern (string-upcase name) :keyword) entry)))


(def-cached-parser bib-entry-value?
  (bracket?
   "{"
   (bib-entry-value-rec?)
   "}"))


;; Turn a character into a string; leave strings alone
(defun assert-string (x)
  (if (stringp x)
      x
      (make-string 1 :initial-element x)))


;; most complicated parser
;; Problem: the value of a bib-item it delimited by { and }
;; bit it can also contain those. So we need a recursive parser here
(def-cached-parser bib-entry-value-rec?
  (named-seq?
   ;; Parse the contents into a tree of strings and characters
   (<- tree
       (many?
        ;; Either a normal string, or something that starts and ends with "{" and "}"
        (choice
         ;; String
         (gather-if-not* (rcurry 'member '(#\{ #\})))
         ;; Recursive bib-entry
         (seq-list?
          "{"
          (bib-entry-value-rec?)
          "}"))))
   (progn
     (apply 'concatenate 'string (mapcar 'assert-string (flatten tree))))))


(defun comment-p (rec)
  (if (eq (first rec) :COMMENT) t))


(defun parse (string)
  "Parse the bibtext content from string. Return result as list of entries (comments are dropped).
An entry is an alist of the form: '((:BIB-ENTRY-TYPE TYPE-STRING)
                                    (:BIB-ENTRY-NAME NAME-STRING)
                                    KW-LISTS)
The first element of that list contains the type of the entry as a string.
The second element of that list contains the name of that entry.
The rest of the alist contains keys as symbols in the keyword-package,
parsed from the keys of the bib-entries.
 All values are simple strings."
  (remove-if #'comment-p (parse-string* (bib-file?) string)))


(defun doi-from-bib (bib-record)
  "Gets a :DOI string out from a parsed bibtex entry"
  (second (assoc :DOI bib-record)))


(defun build-records-table (bib-file-path)
  "Reads and parses BibTex file under BIB-FILE-PATH. Then builds the hash table of the records
where :DOI strings are used as keys. Records with no :DOI in parsed bibtex keys are
not included."
  (let ((parsed-records-table (make-hash-table :test 'equal)))
    (loop
      :for rec :in (parse (uiop:read-file-string bib-file-path))
      :do (let ((doi (doi-from-bib rec)))
            (when doi
              (setf (gethash doi parsed-records-table) rec))))
    parsed-records-table))


(defun append-cite-to-rec (cite rec)
  (let ((place (assoc :cites rec)))
    (if place
        (nconc place (list cite))
        (nconc rec (list (list :cites cite))))))


(defvar cites-lock (bt:make-lock))

(defvar entry-count-lock (bt:make-lock))

(defvar num-api-threads 8)

(lparallel:make-kernel num-api-threads :name "custom-kernel")

;; (setf num-api-threads 6)


;; (defun start-api-kernel ()
;;   (setf lparallel:*kernel*
;;         (lparallel:make-kernel num-api-threads :name "custom-kernel")))

;; (start-api-kernel)

;; (end-api-kernel)

;; (defun end-api-kernel ()
;;   (lparallel:end-kernel :wait t))


(defun append-cites-in-table (records-table)
  "Based on the metadata queried from CrossRef, update the records in RECORDS-TABLE
by appending '(:CITES ...) to the record assoc list. Rest of '(:CITES ...) contains
citation hash-tables with :DOI entries that the publication cites and that are also
keys in the same RECORDS-TABLE."
  ;; (start-api-kernel)                    ; spawn threads knocking on CrossRef
  (format t "Fetching citations metadata across ~a threads...~%" num-api-threads)
  (let ((total-count (hash-table-count records-table))
        (entry-count 0))
    ;; generate API requests as futures:
    (loop
      :for key :being :the hash-keys :of records-table
      :do (let ((rec (gethash key records-table)))
            (let ((resp (lparallel:promise)))
              (lparallel:future
                (lparallel:fulfill
                    resp (jonathan:parse
                          (dex:get (format nil "https://api.crossref.org/works/~a"
                                           (doi-from-bib rec)))
                          :as :hash-table))
                (lparallel:force
                 (lparallel:future
                   (bt:with-lock-held (cites-lock) ; blockingly update citations of the entry record being processed
                     (loop :for ref :in (gethash "reference" (gethash "message" (lparallel:force resp)))
                           :do (let ((cite-doi (gethash "DOI" ref)))
                                 (if (and cite-doi (gethash cite-doi records-table))
                                     (append-cite-to-rec ref rec)))))
                   ;; blockingly increase number of processed threads
                   (bt:with-lock-held (entry-count-lock)
                     (incf entry-count)
                     (format t "[~a/~a] Fetched citations for entry:  ~a~%"
                             entry-count total-count (doi-from-bib rec)))
                   ))))))

    ;; wait till all promises are fulfilled:
    (loop :until (= total-count entry-count)
          :do (sleep 0.001)))
  ;; (lparallel:end-kernel)
  )


(defun process-entries (bib-file-path)
  "Return final processed hash table after parsing BibTex file under BIB-FILE-PATH
and quering extra metadata from the web. Keys are DOI strings, values - parsed and
updated entries as alists."
  (let ((res-table (build-records-table bib-file-path)))
   (-> res-table
       (append-cites-in-table))
   res-table))


(defun graph-open ()
  "Output header of the grapviz-dot graph."
  "digraph PaperGraph {
graph [truecolor=true, bgcolor=\"#ffffff01\"];
node [shape=box, style=filled, color=\".7 .3 1.0\"];
edge [color=\"cyan\"];")


(defun append-graph-close (string)
  (format nil "~a~%}" string))


(defun assoc-or-empty (key rec &optional tag)
  (let ((val (second (assoc key rec))))
    (if val
        (if tag (format nil "<~a>~a</~a>"
                        tag val tag)
            (format nil "~a" val))
        (format nil ""))))


(defun graph-node-edges (rec)
  (loop :for reftable :in (rest (assoc :cites rec))
        :collect (format nil "\"~a\" -> \"~a\" [tooltip=\"~a\"];"
                         (second (assoc :doi rec))
                         (gethash "DOI" reftable)
                         (gethash "key" reftable))))


(defun graph-node (rec)
  (format nil "\"~a\" [label=<(~a) ~a<br/>~a<br/>~a<br/>~a>];~%~{~A~%~}"
          (assoc-or-empty :doi rec)
          (assoc-or-empty :year rec)
          (assoc-or-empty :title rec)
          (assoc-or-empty :author rec "b")
          (assoc-or-empty :journal rec "i")
          (assoc-or-empty :url rec "u")
          (graph-node-edges rec)))


(defun append-graph-nodes (string records-table)
  (let ((nodes-list
          (loop :for rec :being :the hash-values of records-table
                :collect (graph-node rec))))
    (format nil "~a~%~{~a~%~}" string nodes-list)))


(defun process-graph (records-table)
  "Convert processed BibTex entries from RECORDS-TABLE to a Dot-graph."
  (-> (graph-open)
      (append-graph-nodes records-table)
      (append-graph-close)))
