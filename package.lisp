(defpackage :vtf
    (:export #:is #:is-true #:is-false #:fail #:signals
             #:test-failure
             #:*test-verbose*
             #:*fixture* #:*last-fixture*
             #:test #:deftest
             #:define-fixture #:with-fixture #:setup-fixture
             #:def-suite #:in-suite
             #:setup #:teardown #:run-fixture-test-case
             #:run-test-item
             #:run-tests
             #:*verbose-test-logging*
             #:logged-fixture #:expecting #:<< #:<<< #:==>)
  (:use :cl :alexandria))
