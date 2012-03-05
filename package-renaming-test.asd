;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(asdf:defsystem :package-renaming-test
  :description "testing package-renaming"
  :around-compile "asdf::call-with-package-renaming-test-renamings"
  :depends-on (:package-renaming :hu.dwim.stefil)
  :components
  ((:file "package-renaming-test")))

;; IMPORTANT: see the README about the constraints on compile-time renamings.
(defun asdf::call-with-package-renaming-test-renamings (thunk)
  (funcall (asdf::find-symbol* '#:call-with-effective-package-renamings :package-renaming)
           '((:stefil :stefil.non-dwim)
             (:hu.dwim.stefil (:hu.dwim.stefil :stefil)))
           thunk))
