#+xcvb (module nil)

(cl:defpackage #:package-renaming
  (:use #:common-lisp #:alexandria)
  (:export
   #:check-same-package-names
   #:check-package-candidate-names
   #:package-names
   #:find-package-from-names
   #:effect-package-renaming
   #:effect-package-renamings
   #:call-with-package-renamings
   #:with-package-renamings))

(in-package :package-renaming)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun check-same-package-names (names)
  "Check that the NAMES all designate the same package"
  (check-type names cons)
  (let* ((first-name (first names))
         (other-names (rest names))
         (package (find-package first-name)))
    (loop :for name :in other-names :do
      (assert (eq package (find-package name))))
    (values)))

(defun check-package-candidate-names (package-designator names)
  "Check that each of the NAMES either designates the package
specified by PACKAGE-DESIGNATOR or no package at all"
  (check-type names cons)
  (loop :with package = (find-package package-designator)
    :for name :in names
    :for p = (find-package name)
    :unless (or (null p) (eq p package)) :do
    (error "~S designates a package different from ~S"
           name package-designator))
  (values))

(defun package-names (package-designator)
  (let ((package (find-package package-designator)))
    (when package
      (cons (package-name package) (package-nicknames package)))))

(defun find-package-from-names (names &key if-does-not-exist)
  (let ((package (some #'find-package names)))
    (or package
        (ecase if-does-not-exist
          ((nil) nil)
          ((error) (error "No package named any of ~S" names))))))

(defun effect-package-renaming (old new &key if-does-not-exist)
  (let* ((old-names (mapcar #'string (ensure-list old)))
         (new-names (mapcar #'string (ensure-list new)))
         (package (find-package-from-names
                   old-names :if-does-not-exist if-does-not-exist)))
    (when package
      (check-package-candidate-names package old-names)
      (check-package-candidate-names package new-names)
      (let ((previous-names (when package (package-names package))))
        (rename-package package (first new-names) (rest new-names))
        previous-names))))

(defun effect-package-renamings (package-renamings &key if-does-not-exist)
  (reverse
   (loop :for (old new) :in package-renamings
     :collect (list new (or (effect-package-renaming
                             old new :if-does-not-exist if-does-not-exist) old)))))

(defun call-with-package-renamings (package-renamings thunk &key if-does-not-exist)
  (let ((reverse-renamings
         (effect-package-renamings package-renamings
                                   :if-does-not-exist if-does-not-exist)))
    (unwind-protect
         (funcall thunk)
      (effect-package-renamings reverse-renamings
                                :if-does-not-exist if-does-not-exist))))

(defmacro with-package-renamings ((package-renamings &key if-does-not-exist) &body body)
  `(call-with-package-renamings
    ,package-renamings #'(lambda () ,@body) :if-does-not-exist ,if-does-not-exist))

);eval-when