(defpackage papergraph
  (:use :cl :parser-combinators :alexandria :arrows)
  (:export #:process-entries #:process-graph))
(in-package :papergraph)


(defvar clusters-only nil)

(defvar clusters nil)

(defvar unclustered-color ".7 .3 1.0")

(defvar edge-color "cyan")

(defvar layered-view nil)


(defun split-me (str)
  (uiop:split-string str :separator '(#\Space #\Tab #\,)))


(defun clean-me (str)
  (remove-if (lambda (c) (or (char-equal #\Newline c)
                             (char-equal #\' c)
                             (char-equal #\" c)
                             (char-equal #\\ c)
                             (char-equal #\{ c)
                             (char-equal #\} c))) str))


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


(defun split-keywords-in (rec)
  "Destructively splits string parsed as :KEYWORDS in REC alist into separate word strings."
  (setf (assoc-value rec :keywords)
        (-<> (car (assoc-value rec :keywords))
             (remove-if (lambda (c) (or (char-equal #\Space c)
                                        (char-equal #\Tab c))) <>)
             (split-me <>))))


(defun doi-from-bib (bib-record)
  "Gets a :DOI string out from a parsed bibtex entry"
  (second (assoc :DOI bib-record)))


(defun clusters-present-in (rec)
  "Both predicate and REC updater."
  (let ((cluster-names (mapcar #'car clusters)))
    (loop
      :for name :in cluster-names
      :when (find name (assoc-value rec :keywords) :test #'string-equal)
        :do (progn
              (nconc rec (list (list :dot-node-color
                                     (getf (assoc-value clusters name) :color))))
              (nconc rec (list (list :dot-node-cluster name)))
              (return name)))))


(defun append-unclustered (rec)
  "Both predicate and REC updater."
  (when (not clusters-only)
    (nconc rec (list (list :dot-node-color unclustered-color)))))


(defun build-records-table (parsed-records-table bib-file-path)
  "Reads and parses BibTex file under BIB-FILE-PATH. Then appends to the PARSED-RECORDS-TABLE
where :DOI strings are used as keys. Also modifies :KEYWORDS of the record and appends
:DOT-NODE-COLOR with color that corresponds to the first found cluster name in keywords
or with UNCLUSTERED-COLOR (if unclustered nodes are allowed).
Records with no :DOI in parsed bibtex keys are not included.
Records with no found cluster name in :KEYWORDS when CLUSTERS-ONLY are not included."
                                        ;(let ((parsed-records-table (make-hash-table :test 'equal)))
  (loop
    :for rec :in (parse (uiop:read-file-string bib-file-path))
    :do (progn
          (split-keywords-in rec)   ; edit :keywords collection in the record
          (let ((doi (doi-from-bib rec)))
            ;; those predicates will also append :dot-node-color to record:
            (when (and doi (or (clusters-present-in rec) (append-unclustered rec)))
              (setf (gethash doi parsed-records-table) rec))))) ; put the record into the table
  parsed-records-table)



(defun append-cite-to-rec (cite rec)
  (let ((place (assoc :cites rec)))
    (if place
        (nconc place (list cite))
        (nconc rec (list (list :cites cite))))))


(defvar cites-lock (bt:make-lock))

(defvar entry-count-lock (bt:make-lock))


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


(defun process-entries (bib-file-path-list)
  "Return final processed hash table after parsing BibTex files under BIB-FILE-PATH
and quering extra metadata from the web. Keys are DOI strings, values - parsed and
updated entries as alists."
  (let ((res-table (make-hash-table :test 'equal)))
    (loop :for bib-file-path :in bib-file-path-list
          :do (build-records-table res-table (truename bib-file-path)))
    (-> res-table
        (append-cites-in-table))
    res-table))


(defun select-for-cluster (name records-table)
  (loop
    for v being the hash-values of records-table
    when (string-equal name (car (assoc-value v :dot-node-cluster)))
      collect v))


(defun select-unclustered (records-table)
  (loop
    for v being the hash-values of records-table
    when (not (assoc-value v :dot-node-cluster))
      collect v))


(defun graph-open ()
  "Output header of the grapviz-dot graph."
  (format nil "digraph PaperGraph {
graph [layout=~a, truecolor=true, bgcolor=\"#ffffff01\", overlap=false, splines=true];
node [shape=box, style=filled, color=\"~a\"];
edge [arrowhead=\"vee\", color=\"~a\", penwidth=1.5, arrowsize=1.5];"
          (if layered-view "dot" "fdp")
          unclustered-color edge-color))


(defun append-graph-close (string)
  (format nil "~a~%}" string))


(defun assoc-or-empty (key rec &optional tag)
  (let ((val (second (assoc key rec))))
    (if val
        (if tag (format nil "<~a>~a</~a>"
                        tag val tag)
            (format nil "~a" val))
        (format nil ""))))


(defun graph-node (rec)
  (format nil "\"~a\" [constraint=false,color=\"~a\",
label=<(~a) ~{~A <br/>~}~{~A <br/>~}~a<br/>~a >];~%"
          (assoc-or-empty :doi rec)
          (assoc-or-empty :dot-node-color rec)
          (assoc-or-empty :year rec)
          (mapcar (lambda (l) (format nil "~{~A ~}" l))
                  (serapeum:batches
                   (split-me (clean-me (assoc-or-empty :title rec))) 8))
          (mapcar (lambda (l) (format nil "<b>~{~A ~}</b>" l))
                  (serapeum:batches
                   (split-me (clean-me (assoc-or-empty :author rec))) 10))
          (assoc-or-empty :journal rec "i")
          (assoc-or-empty :url rec "u")))


(defun graph-node-edges (rec)
  (loop :for reftable :in (rest (assoc :cites rec))
        :collect (format nil "\"~a\" -> \"~a\" [color=\"~a\", tooltip=\"~a\"];"
                         (second (assoc :doi rec))
                         (gethash "DOI" reftable)
                         (second (assoc :dot-node-color rec))
                         (gethash "key" reftable))))


(defun append-edges (string records-table)
  (format nil "~a~%~{~a~}"
          string
          (loop :for rec :being :the hash-values :of records-table
                :collect (format nil "~{~a~%~}" (graph-node-edges rec)))))


(defun append-subgraph (string cluster-name records-table)
  (let ((nodes-list
          (loop :for rec :in (select-for-cluster cluster-name records-table)
                :collect (graph-node rec))))
    (format nil "~a~%subgraph cluster_~a
{~%fontsize=32;~%label=\"~a\";~%color=\"~a\";~%fontcolor=\"~a\";~%~{~a~%~}}~%"
            string
            cluster-name
            (getf (assoc-value clusters cluster-name :test #'string-equal) :label)
            (getf (assoc-value clusters cluster-name :test #'string-equal) :color)
            (getf (assoc-value clusters cluster-name :test #'string-equal) :color)
            nodes-list)))


(defun append-unclustered-subgraph (string records-table)
  (let ((nodes-list
          (loop :for rec :in (select-unclustered records-table)
                :collect (graph-node rec))))
    (format nil "~a~%~{~a~%~}~%"
            string nodes-list)))


(defun process-graph (records-table)
  "Convert processed BibTex entries from RECORDS-TABLE to a Dot-graph."
  (let ((graph (graph-open)))
    (loop :for cluster-name :in (mapcar #'car clusters)
          :do (setf graph (append-subgraph graph cluster-name records-table)))
    (when (not clusters-only)
      (setf graph (append-unclustered-subgraph graph records-table)))
    (setf graph (append-edges graph records-table))
    (append-graph-close graph)))
