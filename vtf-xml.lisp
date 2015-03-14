(defpackage :vtf-xml
  (:export #:abt-xml-output-mixin)
  (:use :cl :alexandria :vtf))

(in-package :vtf-xml)

(defclass abt-xml-output-mixin () ())

(defparameter *xml-indent-level* 2)

(defun ensure-xml (thing)
  (etypecase thing
    (stp:node thing)
    (string
     (cxml:parse-rod thing (stp:make-builder)))
    ((cons t null)
     (ensure-xml (first thing)))))

(defclass attribute-sorter (cxml:sax-proxy) ())

(defmethod sax:start-element ((handler attribute-sorter) uri lname qname attrs)
  (call-next-method handler uri lname qname
                    (sort (copy-list attrs)
                          #'cxml::rod<
                          :key #'sax:attribute-qname)))

(defun make-attribute-sorter (chained-handler)
  (make-instance 'attribute-sorter :chained-handler chained-handler))

(defun write-xml (data stream &key no-indent-p canonical-p)
  (setf data (ensure-xml data))
  (with-standard-io-syntax
    (let ((sink (make-attribute-sorter
                 (cxml:make-whitespace-normalizer
                  (cxml:make-character-stream-sink
                   stream
                   :indentation (unless (or no-indent-p canonical-p)
                                  *xml-indent-level*)
                   :width 10000
                   :canonical canonical-p)))))
      (etypecase data
        (stp:element
         (sax:start-document sink)
         (stp:serialize data sink)
         (sax:end-document sink))
        (stp:document
         (stp:serialize data sink))))))

(defun canonicalize (data)
  (with-output-to-string (out)
    (write-xml data out :canonical-p t)))

(defmethod abt-compare ((fixture abt-xml-output-mixin) expected actual)
  (equal (when expected (canonicalize expected))
         (when actual (canonicalize actual))))

(defmethod abt-pprint ((fixture abt-xml-output-mixin) data stream)
  (write-xml data stream))

(defmethod abt-load ((fixture abt-xml-output-mixin) path)
  (when-let ((text (funcall vtf::*abt-read-function* path)))
    (cxml:parse-rod text (stp:make-builder))))

(defmethod abt-store ((fixture abt-xml-output-mixin) data path)
  (funcall vtf::*abt-write-function*
           (with-output-to-string (out)
             ;; FIXME: perhaps should indent it after all
             (write-xml data out :no-indent-p t))
           path))

(defmethod abt-file-type ((fixture abt-xml-output-mixin)) "xml")
