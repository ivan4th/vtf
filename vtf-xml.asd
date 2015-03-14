;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-

(defpackage #:vtf-xml.system
  (:use #:cl #:asdf))

(in-package :vtf-xml.system)

(defsystem vtf-xml
  :description "VTF - XML support"
  :author "Ivan Shvedunov <ivan4th@gmail.com>"
  :version "0.1"
  :serial t
  :depends-on (:vtf :alexandria :cxml :cxml-stp)
  :components ((:file "vtf-xml")))
