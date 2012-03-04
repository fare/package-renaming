#+xcvb (module nil)

(cl:defpackage #:package-renaming
  (:use #:common-lisp)
  (:export))

(in-package :package-renaming)

(eval-when (:compile-toplevel :load-toplevel :execute)

);eval-when
