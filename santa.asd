(defsystem "santa"
  :version "0.0.0"
  :author "James Kominick"
  :license "MIT"
  :depends-on (
      "uuid"
      "ironclad"
      "arrow-macros"
      "log4cl"
      "str"
      "hunchentoot"
      "drakma"
      "cl-json"
      "metabang-bind"
      "cl-interpol"
      "cl-ppcre"
      "local-time"
      "trivial-backtrace"
      "bordeaux-threads"
      "uiop"
      "alexandria"
      "chanl"
      "cl-permutation"
      "cl-dotenv"
  )
  :serial t
  :components ((:module "src"
                :components
                ((:file "utils")
                 (:file "config")
                 (:file "people")
                 (:file "main")
                 )))
  :description ""
  :in-order-to ((test-op (test-op "santa/tests"))))

(defsystem "santa/tests"
  :author "James Kominick"
  :license "MIT"
  :depends-on ("santa"
               "fiveam")
  :serial t
  :components ((:module "tests"
                :components
                ((:file "main")
                 (:file "test-santa")
                 )))
  :description "Test system for santa"
  :perform (test-op (op sys)
             (symbol-call
               :fiveam :run! (find-symbol* :all :santa/tests))))

