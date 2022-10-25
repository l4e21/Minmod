(defsystem "minmod"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("serapeum" "alexandria" "trivia")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "minmod/tests"))))

(defsystem "minmod/tests"
  :author ""
  :license ""
  :depends-on ("minmod"
               "fiveam")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for minmod"
  :perform (test-op (o s) (symbol-call :minmod/tests/main '#:test-main-suite)))

