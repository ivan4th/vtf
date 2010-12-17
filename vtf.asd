;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-

(defpackage #:vtf.system
  (:use #:cl #:asdf))

(in-package :vtf.system)

(defsystem vtf
  :description "Another test framework"
  :author "Ivan Shvedunov <ivan4th@gmail.com>"
  :version "0.1"
  :serial t
  :depends-on (:alexandria)
  :components ((:file "package")
               (:file "vtf")))
