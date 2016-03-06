;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(defsystem "package-renaming"
  :version "1.0.0"
  :description "locally renaming packages"
  :author "Francois-Rene Rideau"
  :license "MIT"
  :depends-on ("alexandria")
  :components ((:file "package-renaming"))
  :in-order-to ((test-op (test-op "package-renaming-test"))))
