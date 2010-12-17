(defpackage :vtf
    (:export #:is #:is-true #:is-false #:fail #:signals #:test #:def-suite #:in-suite
             #:run-tests #:run-all-tests)
  (:use :cl :alexandria))