(asdf:load-system :papergraph)

(in-package :papergraph)


(defvar num-api-threads 8)

(defvar ref-files '("~/Refs/refs.bib"))

(defvar out-dir #P"/tmp/papergraph/")

(defvar out-file "papergraph.dot")

(defvar system-img-browser nil)


(defun app-run ()
  (format t "Starting PaperGraph application...~%")
  (load #P"papergraphrc")
  (format t "Configuration file loaded~%")

  (setf lparallel:*kernel*
        (lparallel:make-kernel num-api-threads :name "custom-kernel"))
  (uiop:with-current-directory (out-dir)
    (let ((out-file-name (merge-pathnames out-file))
          (out-svg-name (merge-pathnames
                         (format nil "~a.~a" (pathname-name out-file) "svg"))))
      (with-open-file
          (f out-file-name :direction :output :if-exists :supersede)
        (format f (process-graph (process-entries ref-files))))
      (format t "PaperGraph: processing finished!~%")

      (uiop:run-program (format nil "dot -Tsvg -o ~a ~a" out-svg-name out-file-name))
      (format t "PaperGraph: image produced.~%")

      (when system-img-browser
        (progn
          (format t "PaperGraph: starting preview...~%")
          (uiop:run-program (format nil "~a ~a" system-img-browser out-svg-name))))))
  (lparallel:end-kernel :wait t)
  (format t "PaperGraph: done.~%"))


(sb-ext:save-lisp-and-die #P"papergraph"
                          :toplevel #'papergraph::app-run
                          :executable t)
