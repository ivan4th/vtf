;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-

(defpackage #:vtf-test.system
  (:use #:cl #:asdf))

(in-package :vtf-test.system)

(defsystem vtf-test
  :description "Another test framework (tests)"
  :author "Ivan Shvedunov <ivan4th@gmail.com>"
  :version "0.1"
  :serial t
  :depends-on (:alexandria :vtf)
  :components ((:file "test-package")
               (:file "vtf-test")))
