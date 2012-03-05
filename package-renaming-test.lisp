#+xcvb (module (:depends-on ("memoization" (:asdf "hu.dwim.stefil"))))

(defpackage :package-renaming-test (:use :cl :package-renaming :hu.dwim.stefil))
(defpackage :package-renaming-test.2 (:use :cl))

(in-package :package-renaming-test)

(declaim (optimize (speed 1) (debug 3) (space 3)))

(defsuite* (test-suite
            :in stefil:root-suite ;; THIS safely test the package renaming in our .asd
            :documentation "Testing package renaming"))

(deftest test-renaming ()
  (let ((package (find-package :package-renaming-test))
        (package2 (find-package :package-renaming-test.2)))
    (is (equal (package-names package) '("PACKAGE-RENAMING-TEST")))
    (is (equal (find-package "PRT1") nil))
    (is (equal (find-package "PRT2") nil))
    (is (equal (find-package "PRT3") nil))
    (with-effective-package-renamings (((:package-renaming-test :prt1) (:prt2 :prt3))
                                       (:package-renaming-test.2 :package-renaming-test))
      (is (equal (package-names package) '("PRT2" "PRT3")))
      (is (not (eq (find-package :package-renaming-test) package)))
      (is (eq (find-package :package-renaming-test) package2)))
    (is (equal (package-names package) '("PACKAGE-RENAMING-TEST")))))
