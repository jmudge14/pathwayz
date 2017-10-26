(in-package :cl-user)
(defpackage pathwayz-test-asd
  (:use :cl :asdf))
(in-package :pathwayz-test-asd)

(defsystem pathwayz-test
  :author "Jack Mudge"
  :license ""
  :depends-on (:pathwayz
               :prove)
  :components ((:module "t"
                :components
                ((:file "pathwayz"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
