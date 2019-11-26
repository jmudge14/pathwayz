(in-package :cl-user)
(defpackage pathwayz-asd
  (:use :cl :asdf))
(in-package :pathwayz-asd)

(defsystem pathwayz
  :version "0.1"
  :author "Jack Mudge"
  :license ""
  :depends-on (:clack
               :lack
               :caveman2
               :envy
               :cl-ppcre
               :uiop
               :cl-store

               ;; for @route annotation
               :cl-syntax-annot

               ;; HTML Template
               :djula

               ;; for DB
               :datafly
               :sxql)
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view" "board"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "board")
                 (:file "learn" :depends-on ("sigmoid"))
                 (:file "sigmoid")
                 (:file "config"))))
  :description ""
  :in-order-to ((test-op (load-op pathwayz-test))))
