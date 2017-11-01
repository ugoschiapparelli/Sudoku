(cl:in-package #:common-lisp-user)

(asdf:defsystem :mini-gui
  :depends-on (:hunchentoot :sudoku :cl-who)
  :serial t
  :components
  ((:file "package-gui")
   (:file "mini-gui")
   ))
