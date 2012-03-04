#+xcvb (module (:depends-on ("memoization" (:asdf "hu.dwim.stefil"))))

(defpackage :package-renaming-test (:use :cl :package-renaming :hu.dwim.stefil))

(in-package :package-renaming-test)

(declaim (optimize (speed 1) (debug 3) (space 3)))

(defsuite* (test-suite
            :in root-suite
            :documentation "Testing package renaming"))

(deftest test-renaming ()
  (let ((package (find-package :package-renaming-test)))
    (is (equal (package-names package) '("PACKAGE-RENAMING-TEST")))
    (is (equal (find-package "PRT1") nil))
    (is (equal (find-package "PRT2") nil))
    (is (equal (find-package "PRT3") nil))
    (with-package-renamings ('(((:package-renaming-test :prt1) (:prt2 :prt3))
                               (:prt4 :prt5))
                              :if-does-not-exist nil)
      (is (equal (package-names package) '("PRT2" "PRT3")))
      (is (eq (find-package :package-renaming-test) ())))
    (is (equal (package-names package) '("PACKAGE-RENAMING-TEST")))))
