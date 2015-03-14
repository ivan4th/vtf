;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-

(defpackage #:vtf-json.system
  (:use #:cl #:asdf))

(in-package :vtf-json.system)

(defsystem vtf-json
  :description "VTF - JSON support"
  :author "Ivan Shvedunov <ivan4th@gmail.com>"
  :version "0.1"
  :serial t
  :depends-on (:vtf :alexandria :st-json)
  :components ((:file "vtf-json")))
