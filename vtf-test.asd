;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-

(defpackage #:vtf-test.system
  (:use #:cl #:asdf))

(in-package :vtf-test.system)

(defsystem vtf-test
  :description "A test framework with support for Adaptive Baseline Testing (tests)"
  :author "Ivan Shvedunov <ivan4th@gmail.com>"
  :version "1.0"
  :serial t
  :depends-on (:alexandria :vtf :vtf-xml)
  :components ((:file "test-package")
               (:file "vtf-test")))
