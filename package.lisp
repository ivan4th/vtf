(defpackage :vtf
    (:export #:is
             #:is-true
             #:is-false
             #:fail
             #:signals
             #:check-failed
             #:check-passed
             #:test-failure
             #:*test-verbose*
             #:*fixture*
             #:*last-fixture*
             #:*redirect-test-output*
             #:test #:deftest
             #:define-fixture
             #:with-fixture
             #:setup-fixture
             #:def-suite #:in-suite
             #:setup
             #:teardown
             #:run-fixture-test-case
             #:invoke-test-case
             #:run-test-item
             #:run-tests
             #:*verbose-test-logging*
             #:logged-fixture
             #:expecting
             #:<<
             #:<<<
             #:==>
	     #:===
             #:abt-compare
             #:abt-pprint
             #:abt-load
             #:abt-store
             #:abt-file-type
             #:abt-diff
             #:abt-data-location
             #:abt-emit
             #:abt-accept
             #:abt-clear
             #:abt-rediff
             #:with-abt-section
             #:abt-text-output-mixin
             #:abt-lisp-output-mixin
             #:abt-fixture)
  (:use :cl :alexandria))
