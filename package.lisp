(defpackage :vtf
    (:export #:is #:is-true #:is-false #:fail #:signals
             #:*fixture* #:*last-fixture*
             #:test #:deftest
             #:define-fixture #:with-fixture #:setup-fixture
             #:def-suite #:in-suite
             #:setup #:teardown
             #:run-tests #:run-all-tests
             #:logged-fixture #:expecting #:<< #:<<< #:==>)
  (:use :cl :alexandria))
