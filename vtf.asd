;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-

(defpackage #:vtf.system
  (:use #:cl #:asdf))

(in-package :vtf.system)

(defsystem vtf
  :description "A test framework with support for Adaptive Baseline Testing"
  :author "Ivan Shvedunov <ivan4th@gmail.com>"
  :version "1.0"
  :serial t
  :depends-on (:alexandria :cl-difflib :cl-ppcre :babel :split-sequence)
  :components ((:file "package")
               (:file "core")
               (:file "deftest")
               (:file "assertions")
               (:file "logged")
               (:file "abt")
               (:file "run-tests")))
