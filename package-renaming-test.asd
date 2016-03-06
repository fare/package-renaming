;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(defsystem "package-renaming-test"
  :version "1.0.0"
  :description "testing package-renaming"
  :author "Francois-Rene Rideau"
  :license "MIT"
  :around-compile "asdf::call-with-package-renaming-test-renamings"
  :depends-on ("package-renaming" "hu.dwim.stefil")
  :components ((:file "package-renaming-test"))
  :perform (test-op (o c) (symbol-call :package-renaming-test :test-suite)))


;; IMPORTANT: see the README about the constraints on compile-time renamings.
(defun asdf::call-with-package-renaming-test-renamings (thunk)
  (symbol-call :package-renaming '#:call-with-effective-package-renamings
               '((:stefil :stefil.non-dwim)
                 (:hu.dwim.stefil (:hu.dwim.stefil :stefil)))
               thunk))
