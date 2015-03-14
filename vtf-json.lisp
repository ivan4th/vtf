(defpackage :vtf-json
  (:export #:abt-json-output-mixin)
  (:use :cl :alexandria :vtf))

(in-package :vtf-json)

(defclass abt-json-output-mixin () ())

;; reparse to account for differences arising from
;; floating point representation
(defun reparse-json (thing)
  (st-json:read-json-from-string
   (if (stringp thing)
       thing
       (st-json:write-json-to-string thing))))

(defun jso->sorted-plist (jso)
  (let ((alist '()))
    (st-json:mapjso #'(lambda (k v) (push (cons k v) alist)) jso)
    (alist-plist (sort alist #'string< :key #'car))))

(defun json-equal (a b)
  (cond ((and (typep a 'st-json:jso) (typep b 'st-json:jso))
         (json-equal (jso->sorted-plist a) (jso->sorted-plist b)))
        ((not (and (listp a) (listp b)))
         (equal a b))
        ((length= a b)
         (every #'json-equal a b))
        (t nil)))

(defun normalize-json (thing)
  (labels ((frob (json)
             (typecase json
               (st-json:jso
                (apply #'st-json:jso
                       (mapcar #'frob (jso->sorted-plist json))))
               (list
                (mapcar #'frob json))
               (t json))))
    (frob (reparse-json thing))))

(defmethod abt-compare ((fixture abt-json-output-mixin) expected actual)
  (json-equal (reparse-json expected) (reparse-json actual)))

(defmethod abt-pprint ((fixture abt-json-output-mixin) data stream)
  (st-json:write-json (normalize-json data) stream :pretty t))

(defmethod abt-load ((fixture abt-json-output-mixin) path)
  (when-let ((text (funcall vtf::*abt-read-function* path)))
    (st-json:read-json-from-string text)))

(defmethod abt-store ((fixture abt-json-output-mixin) data path)
  (funcall vtf::*abt-write-function*
           (st-json:write-json-to-string (normalize-json data) :pretty t)
           path))

(defmethod abt-file-type ((fixture abt-json-output-mixin)) "json")
