(defsystem "papergraph"
  :version "0.1.0"
  :author "Vladimir Dikan"
  :license "MIT"
  :depends-on ("parser-combinators"
               "alexandria"
               "dexador"
               "jonathan")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "Bibtex -> Graphviz-Dot converter. Using CrossRef to grab references."
  :in-order-to ((test-op (test-op "papergraph/tests"))))

(defsystem "papergraph/tests"
  :author "Vladimir Dikan"
  :license "MIT"
  :depends-on ("papergraph"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for papergraph"
  :perform (test-op (op c) (symbol-call :rove :run c)))
