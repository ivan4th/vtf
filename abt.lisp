(in-package :vtf)

;;; Adaptive Baseline Testing

;; TBD: it should be possible to specify per-testcase types
;; need to add ABT-OUTPUT method that can be specialized
;; on fixtures and test cases and returns class name of
;; output mode

(defgeneric abt-compare (fixture expected actual)
  (:documentation "Compare expected data with actual data.
  Add methods specializing on FIXTURE to support custom
  data types."))

(defgeneric abt-pprint (fixture data stream)
  (:documentation "Pretty-print data. Add methods specializing on
  FIXTURE to support custom data types."))

(defgeneric abt-load (fixture path)
  (:documentation "Load data file specified by PATH. Add methods
  specializing on FIXTURE to support custom data types."))

(defgeneric abt-store (fixture data path)
  (:documentation "Store data into the file with specified PATH. Add
  methods specializing on FIXTURE to support custom data types."))

(defgeneric abt-file-type (fixture)
  (:documentation "Return file type (extension) used for ABT data
  files of FIXTURE"))

(defvar *abt-read-function*
  #'(lambda (path)
      (handler-case
          (babel:octets-to-string
           (read-file-into-byte-vector path))
        (file-error () nil))))
(defvar *abt-write-function*
  #'(lambda (data path)
      (write-byte-vector-into-file
       (babel:string-to-octets data)
       path :if-exists :supersede)))
(defvar *abt-del-function*
  #'(lambda (path)
      (delete-file path)))
(defvar *abt-dir-function*
  #'(lambda (path)
      (directory (merge-pathnames
                  (make-pathname :name :wild :type :wild)
                  path))))
(defvar *abt-path*)
(defvar *abt-section*)
(defvar *abt-diff-items* '())
(defvar *abt-missing* '())
(defvar *abt-note-missing*)

(defclass abt-text-output-mixin () ())

(defun preprocess-text (text)
  (format nil "~{~a~}" (mapcar #'princ-to-string (ensure-list text))))

(defmethod abt-compare ((fixture abt-text-output-mixin) expected actual)
  (equal (preprocess-text expected)
         (preprocess-text actual)))

(defmethod abt-pprint ((fixture abt-text-output-mixin) data stream)
  (write-string (preprocess-text data) stream))

(defmethod abt-load ((fixture abt-text-output-mixin) path)
  (funcall *abt-read-function* path))

(defmethod abt-store ((fixture abt-text-output-mixin) data path)
  (funcall *abt-write-function* (preprocess-text data) path))

(defmethod abt-file-type ((fixture abt-text-output-mixin)) "dat")

(defclass abt-lisp-output-mixin () ())

(defmethod abt-compare ((fixture abt-lisp-output-mixin) expected actual)
  (equal expected actual))

(defmethod abt-pprint ((fixture abt-lisp-output-mixin) data stream)
  (with-standard-io-syntax
      (dolist (item data)
        (write (unbase item)
               :stream stream
               :pretty t
               :circle t
               :right-margin 95
               :case :downcase)
        (terpri stream))))

(defmethod abt-load ((fixture abt-lisp-output-mixin) path)
  (when-let ((text (funcall *abt-read-function* path)))
    (with-input-from-string (in text)
      (let ((eof (cons nil nil)))
        (loop for item = (read in nil eof)
           until (eq item eof)
           collect item)))))

(defmethod abt-store ((fixture abt-lisp-output-mixin) data path)
  (funcall *abt-write-function*
           (with-standard-io-syntax
             (with-output-to-string (out)
               (dolist (item data)
                 (write (unbase item)
                        :stream out
                        :pretty t
                        :right-margin 95
                        :case :downcase)
                 (terpri out))))
           path))

(defmethod abt-file-type ((fixture abt-lisp-output-mixin)) "dat")

(defun abt-file-name (name fixture)
  (concatenate 'string
               (cl-ppcre:regex-replace-all
                "[\\\\/:]"
                (etypecase name
                  (string name)
                  (symbol (string-downcase name)))
                "-")
               "." (abt-file-type fixture)))

(defun abt-path (name fixture)
  (merge-pathnames (abt-file-name name fixture) *abt-path*))

(defmacro with-abt-section ((path &key (note-missing t) (diff-p t)) &body body)
  (once-only (path note-missing)
    `(let ((*abt-path* ,path)
           (*abt-section* '()))
       ,@(if diff-p
             `(,@body (abt-diff :note-missing ,note-missing))
             body))))

(defun abt-emit (data name &optional (fixture *fixture*))
  (setf *abt-section*
        (cons (list name data fixture)
              (delete name *abt-section* :key #'first))))

(defun report-abt-diff (name fixture expected actual)
  (flet ((format-data->list (data)
           (cl-ppcre:split "[ \\t]*\\n"
                           (with-output-to-string (out)
                             (abt-pprint fixture data out)))))
    (signal 'check-failed
            :name name
            :kind (if expected :diff :new)
            :cause (with-output-to-string (diff-out)
                     (difflib:unified-diff
                      diff-out
                      (when expected (format-data->list expected))
                      (format-data->list actual)
                      :test-function #'equal
                      :from-file "expected" :to-file "actual")))))

(defun abt-rediff (&key (filter-expected #'identity)
                     (filter-actual #'identity)
                     (filter #'identity))
  (loop for (path name actual-orig fixture) in (reverse *abt-diff-items*)
        for actual = (funcall filter-actual
                              (funcall filter actual-orig))
        for expected = (funcall filter-expected
                                (funcall filter (abt-load fixture path)))
        unless (abt-compare fixture expected actual)
          do (handler-case
                 (report-abt-diff name fixture expected actual)
               (check-failed (c)
                 (display-result (condition-result c) *debug-io*)))
        end))

(defun abt-diff (&key note-missing)
  (let* ((all-files (when note-missing
                      (mapcar #'file-namestring (funcall *abt-dir-function* *abt-path*))))
         (pass-p t)
         (found
           (handler-bind ((check-failed
                            #'(lambda (c)
                                (unless (handled-p c)
                                  (setf pass-p nil)))))
             (loop for (name actual fixture) in (reverse *abt-section*)
                   for path = (abt-path name fixture)
                   for expected = (abt-load fixture path)
                   collect (abt-file-name name fixture)
                   when (abt-compare fixture expected actual)
                     do (check-passed name)
                        (deletef *abt-diff-items* name :key #'second)
                   else
                     do (report-abt-diff name fixture expected actual)
                        (deletef *abt-diff-items* name :test #'equal :key #'second)
                        (push (list path name actual fixture) *abt-diff-items*)
                   end))))
    (let* ((missing (when note-missing (set-difference all-files found :test #'equal)))
           (name (if *current-test-case* (name *current-test-case*) 'no-test-case))
           (signalled-p (find name *abt-section* :key #'first :test #'equal)))
      (cond (missing
             (nunionf *abt-missing*
                      (loop for filename in missing
                            collect (merge-pathnames filename *abt-path*))
                      :test #'equal)
             (unless signalled-p
               (check-failed name :missing-items
                             (format nil "~{~a~^~%~}" missing))))
            (signalled-p nil)
            ((not pass-p)
             (check-failed name :missing-items
                           (format nil "~{~a~^~%~}" missing)))
            (t
             (check-passed name))))))

(defun abt-accept (&optional names)
  (flet ((accept (item)
           (destructuring-bind (path name actual fixture) item
             (format *debug-io* "~&;; ACCEPT: ~s --> ~s~%" name path)
             (abt-store fixture actual path))))
    (cond (names
           (loop for name in names
                 do (if-let ((item (find name *abt-diff-items*
                                         :key #'second
                                         :test #'equal)))
                      (accept item)
                      (warn "ABT item ~s not found" name))))
          (t
           (mapc #'accept *abt-diff-items*)
           (loop for path in *abt-missing*
                 do (format *debug-io* "~&;; DELETE: ~s~%" path)
                    (funcall *abt-del-function* path))
           (abt-reset)))))

(defun abt-reset ()
  (setf *abt-diff-items* '() *abt-missing* '()))

;;; ABT fixture

(defgeneric abt-data-location (fixture))

(defun abt-get-location (fixture)
  (let ((loc (abt-data-location fixture)))
    (etypecase loc
      (pathname loc)
      (string (pathname loc))
      (proper-list
       (unless (and (<= 2 (length loc) 3)
                    (eq :asdf (first loc))
                    (typep (second loc) '(or symbol string))
                    (typep (third loc) '(or null string pathname)))
         (error "invalid loc spec ~s" loc))
       (asdf:system-relative-pathname (second loc) (or (third loc) #p"./"))))))

(defclass abt-fixture (abt-lisp-output-mixin logged-fixture)
  ((data-location :reader abt-data-location :initarg :data-location)))

(defmethod invoke-test-case ((fixture abt-fixture) (test-case test-case))
  (with-abt-section ((abt-get-location fixture) :note-missing nil)
    (call-next-method)
    (abt-emit (reverse (log-of fixture)) (name test-case) fixture)))
