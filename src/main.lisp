(defpackage papergraph
  (:use :cl :parser-combinators :alexandria :cl-arrows)
  (:export #:process-entries))
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


(defun append-cites-in-table (records-table)
  "Based on the metadata queried from CrossRef, update the records in RECORDS-TABLE
by appending '(:CITES ...) to the record assoc list. Rest of '(:CITES ...) contains
DOI strings that the publication cites and that are also keys in the same RECORDS-TABLE."
  (loop
    :for key :being :the hash-keys :of records-table
    :do (let* ((rec (gethash key records-table))
               (doi (doi-from-bib rec)))
          (if doi
              (let ((resp (jonathan:parse
                           (dex:get (format nil "https://api.crossref.org/works/~a"
                                            (doi-from-bib rec)))
                           :as :hash-table)))
                (loop
                  :for ref :in (gethash "reference" (gethash "message" resp))
                  :do (let ((cite-doi (gethash "DOI" ref)))
                        (if (and cite-doi (gethash cite-doi records-table))
                            (append-cite-to-rec cite-doi rec)))))))))


(defun process-entries (bib-file-path)
  "Return final processed hash table after parsing BibTex file under BIB-FILE-PATH
and quering extra metadata from the web. Keys are DOI strings, values - parsed and
updated entries as alists."
  (let ((res-table (build-records-table bib-file-path)))
   (-> res-table
       (append-cites-in-table))
   res-table))
