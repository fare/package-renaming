;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(asdf:defsystem :package-renaming-test
  :description "testing package-renaming"
  ;;:around-compile "asdf::call-with-package-renaming-test-renamings"
  :depends-on (:package-renaming :hu.dwim.stefil)
  :components
  ((:file "package-renaming-test")))

#|
;; Oops, for this to work, we would need to:
;; 1- have an :around-load hook around load
;;  so the packages are renamed the same while loading, or
;; 2- have an :around-build hook around both compilation and load,
;; so the packages are renamed the same while both compiling and loading.
;; In any case, the current ASDF hook (as of 2.20) is insufficient. Grrrrrr!
(defun asdf::call-with-package-renaming-test-renamings (thunk)
  (funcall (asdf::find-symbol* '#:call-with-effective-package-renamings :package-renaming)
           '((:stefil :stefil.non-dwim)
             (:hu.dwim.stefil :stefil))
           thunk))
|#
