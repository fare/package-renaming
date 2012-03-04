#+xcvb (module (:depends-on ("memoization" (:asdf "hu.dwim.stefil"))))

(defpackage :package-renaming-test (:use :cl :package-renaming :hu.dwim.stefil))

(in-package :package-renaming-test)

(declaim (optimize (speed 1) (debug 3) (space 3)))

(defsuite* (test-package-renaming
            :in root-suite
            :documentation "Testing package renaming"))

