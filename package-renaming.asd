;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(defsystem :package-renaming
  :description "locally renaming packages"
  :components
  ((:file "package-renaming")))

(defmethod perform ((op test-op) (system (eql (find-system :package-renaming))))
  (asdf:load-system :package-renaming-test)
  (funcall (asdf::find-symbol* :test-suite :package-renaming-test)))
