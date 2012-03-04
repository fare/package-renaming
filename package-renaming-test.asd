;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(asdf:defsystem :package-renaming-test
  :description "testing package-renaming"
  :depends-on (:package-renaming :hu.dwim.stefil)
  :components
  ((:file "package-renaming-test")))
