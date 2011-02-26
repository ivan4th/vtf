(in-package :vtf)

(defvar *test-items* '())
(defvar *test-verbose* t)
(defvar *fixture* nil)
(defvar *last-fixture* nil)
(defvar *keep-fixture* nil)

;;; PROTOCOL

(defgeneric run-test-item (item))

(defgeneric setup (fixture))

(defgeneric teardown (fixture))

(defgeneric run-fixture-test-case (fixture test-case teardown-p))

(defgeneric n-passed (result))

(defgeneric failed-names (result))

(defgeneric display-result (result stream))

(defgeneric aggregate-title (result))

(defgeneric condition-result (condition))

(defgeneric fixture-names (fixture)
  (:method-combination append))

;;; TEST RESULT

(defclass test-result ()
  ((name :reader name
         :initarg :name
         :type (or string symbol null)
         :initform (error "must specify the name of the test result"))))

(defclass single-check-result (test-result) ())

(defclass single-check-success (single-check-result) ())

(defmethod n-passed ((result single-check-success)) 1)

(defmethod failed-names ((result single-check-success)) '())

(defmethod display-result ((result single-check-success) stream)
  (format stream "~&*** PASS: ~s~%" (name result)))

(defclass single-check-failure (single-check-result)
  ((condition :reader result-condition
              :initarg :condition
              :type (or condition null)
              :initform nil)))

(defmethod n-passed ((result single-check-failure)) 0)

(defmethod failed-names ((result single-check-failure))
  (list (name result)))

(defmethod display-result ((result single-check-failure) stream)
  (if (result-condition result)
      (format stream "~&*** FAIL: ~s~%~s: ~a~%---~%"
              (name result) (type-of (result-condition result))
              (result-condition result))
      (format stream "~&*** FAIL: ~s~%" (name result))))

(defclass aggregate-result (test-result)
  ((children :accessor children :initarg :children
             :type list
             :initform '())))

(defmethod n-passed ((result aggregate-result))
  (reduce #'+ (mapcar #'n-passed (children result))))

(defmethod failed-names ((result aggregate-result))
  (reduce #'append (mapcar #'failed-names (children result))))

(defmethod display-result ((result aggregate-result) stream)
  (let ((n-passed (n-passed result))
        (failed (failed-names result)))
    (format stream
            "~&---~%*** Total~@[ in ~A~]: ~s tests, ~s passed, ~s failed~@[: ~s~]~%"
            (aggregate-title result)
            (+ n-passed (length failed))
            n-passed (length failed) failed)))

(defclass fixture-result (aggregate-result) ())

(defmethod aggregate-title ((result fixture-result))
  (format nil "fixture ~a" (name result)))

(defclass package-result (aggregate-result) ())

(defmethod aggregate-title ((result package-result))
  (format nil "package ~a" (name result)))

(defclass global-result (aggregate-result) ())

(defmethod aggregate-title ((result global-result)) nil)

;;; CHECKS

(define-condition check-condition ()
  ((handled-p :accessor handled-p :initform nil)
   (name :reader name :initarg :name
         :initform (error "must specify check condition name"))))

(define-condition check-passed (check-condition) ())

(defmethod condition-result ((condition check-passed))
  (make-instance 'single-check-success
                 :name (name condition)))

(define-condition check-failed (check-condition)
  ((inner-condition :reader inner-condition :initarg :inner-condition
                    :initform nil)))

(defmethod condition-result ((condition check-failed))
  (make-instance 'single-check-failure
                 :name (name condition)
                 :condition (inner-condition condition)))

;;; TEST ITEM

(defclass test-item ()
  ((name :accessor name :type symbol :initarg :name
         :initform (error "must specify test item name"))
   (item-package :accessor item-package :type (or package null)
                 :initarg :item-package
                 :initform (error "must specify the package"))))

(defmethod print-object ((object test-item) stream)
  (print-unreadable-object (object stream :type t)
    (prin1 (name object) stream)))

;;; TEST CASE

(defclass test-case (test-item)
  ((fixture-name :accessor fixture-name :type symbol :initarg :fixture-name
                 :initform (error "must specify fixture name"))
   (test-function :accessor test-function :type symbol :initarg :test-function
                  :initform (error "must specify test function"))))

(defmethod run-fixture-test-case ((fixture t) (test-case test-case) teardown-p)
  (setup fixture)
  (unwind-protect
       (funcall (test-function test-case) fixture)
    (when teardown-p
      (teardown fixture))))

(defmethod run-test-item ((test-case test-case))
  (let ((check-results '())
        (pass-p t))
    (handler-bind ((check-condition
                    #'(lambda (c)
                        (unless (handled-p c)
                          (setf (handled-p c) t)
                          (let ((result (condition-result c)))
                            (when *test-verbose*
                              (display-result result *debug-io*))
                            (push result check-results)
                            (setf pass-p (and pass-p
                                              (null (failed-names result)))))))))
      (let ((*fixture* (or *fixture*
                           (when (fixture-name test-case)
                             (make-instance (fixture-name test-case)))))
            (result-name (name test-case)))
        (setf *last-fixture* *fixture*)
        (cond (*keep-fixture*
               (run-fixture-test-case *fixture* test-case nil))
              (t
               (handler-case
                   (progn
                     (run-fixture-test-case *fixture* test-case t)
                     (signal (if pass-p 'check-passed 'check-failed)
                             :name result-name))
                 (serious-condition (condition)
                   (signal 'check-failed
                           :name result-name
                           :inner-condition condition)))))))
    (nreverse check-results)))

;;; TEST LISTS

(defun run-children (result-class name items)
  (let ((result
         (make-instance
          result-class
          :name name
          :children              
          (loop for item in items
                append (run-test-item
                        (if (consp item)
                            (destructuring-bind (item-type name) item
                              (or (get name item-type)
                                  (error "no such item ~s of type ~s" name item-type)))
                            item))))))
    (when *test-verbose*
      (display-result result *debug-io*))
    (list result)))

;;; FIXTURE

;; TBD: default setup / teardown implementation (should cover nil)

(defclass fixture-test-list (test-item) ())

(defmethod run-test-item ((test-list fixture-test-list))
  (let* ((*fixture* (make-instance (name test-list)))
         (fixture-names (fixture-names *fixture*)))
    (run-children 'fixture-result (name test-list)
                  (sort
                   (loop for item in *test-items*
                         for (item-type name) = item
                         when (and (eq 'test-case item-type)
                                   (member
                                     (fixture-name (get name 'test-case))
                                     fixture-names))
                           collect item)
                   #'string< :key #'second))))

(defmacro define-fixture (name direct-superclasses direct-slots &rest options)
  `(progn
     (defclass ,name ,direct-superclasses ,direct-slots
       ,@(remove :abstract options :key #'first))
     (defmethod fixture-names append ((fixture ,name)) (list ',name))
     ,@(unless (find :abstract options :key #'first)
         `((pushnew (list 'fixture ',name) *test-items* :test #'equal)
           (setf (get ',name 'fixture)
                 (make-instance 'fixture-test-list
                                :name ',name
                                :item-package *package*))))))

(defmethod setup ((fixture t)) nil)

(defmethod teardown ((fixture t)) nil)

;;; PACKAGE

(defmethod run-test-item ((package package))
  (run-children 'package-result (package-name package)
                (sort
                 (loop for item in *test-items*
                       for (item-type name) = item
                       when (eq package
                                (item-package
                                 (or (get name item-type)
                                     (error "bad item ~s of type ~s"
                                            name item-type))))
                         collect item)
                 #'string< :key #'second)))


;;; RUNNING ALL TESTS

(defmethod run-test-item ((item (eql :all)))
  (run-children 'global-result nil
                (sort
                 (remove-duplicates
                  (loop for (item-type name) in *test-items*
                        for item = (get name item-type)
                        when (item-package item)
                          collect (item-package item)))
                 #'string<
                 :key #'package-name)))


;;; CONVENIENCE

(defun run-tests (&optional (name t))
  (cond ((eq t name)
         (run-test-item :all))
        ((get name 'test-case)
         (run-test-item (get name 'test-case)))
        ((get name 'fixture)
         (run-test-item (get name 'fixture)))
        ((find-package name)
         (run-test-item (find-package name)))
        (t
         (error "cannot locate any tests for the specified name: ~s" name))))

;;; TEST and DEFTEST macros

(defmacro test (spec &body body)
  #+nil (simple-style-warning "TEST is retained only for 5am compatibility")
  (destructuring-bind (name &key suite)
      (ensure-list spec)
    (declare (ignore suite)) ;; FIXME: 5am compat
    `(deftest ,name () () ,@body)))

(defun setup-fixture (name)
  (let ((fixture (make-instance name)))
    (setup fixture)
    fixture))

(defmacro with-fixture (slots expr &body body)
  `(let ((*fixture* ,expr)) ;; needed for direct invocation
     ,@(if slots
           `((with-slots ,slots *fixture*
               ,@body))
           body)
     *fixture*))

(defmacro deftest (name slots (&optional fixture-spec &rest options) &body body)
  (declare (ignore options))
  (assert (or (not (null fixture-spec))
              (null slots))
          () "cannot specify slots without fixture")
  (assert (or (null fixture-spec)
              (typep fixture-spec '(or symbol
                                    (cons symbol (cons symbol null)))))
          () "invalid fixture spec ~s" fixture-spec)
  (let ((actual-func-name (symbolicate "%ACTUAL-" name)))
    (multiple-value-bind (fixture-var fixture-name)
        (etypecase fixture-spec
          (null (values (gensym) nil))
          (symbol (values (gensym) fixture-spec))
          (t (values (first fixture-spec) (second fixture-spec))))
      `(progn
         (defun ,actual-func-name (,fixture-var)
           (with-fixture ,slots ,fixture-var ,@body))
         (pushnew (list 'test-case ',name) *test-items* :test #'equal)
         (setf (get ',name 'test-case)
               (make-instance 'test-case
                              :name ',name
                              :fixture-name ',fixture-name
                              :item-package ,(unless fixture-name '*package*)
                              :test-function ',actual-func-name))
         (defun ,name ()
           (let ((*keep-fixture* t))
             (run-test-item (get ',name 'test-case))
             *last-fixture*))))))

;;; ASSERTIONS

(define-condition test-failure (serious-condition) 
  ((message :initform nil :initarg :message :accessor test-failure-message))
  (:report (lambda (condition stream)
	     (format stream
		     "Test failure signalled:~%~a"
		     (test-failure-message condition))))
  (:documentation "Base condition for assertion failures."))

(defun fail (fmt &rest args)
  "Indicate test failure"
  (let ((*print-level* 4) ; FIXME - extract constants
	(*print-length* 100))
    (error 'test-failure :message (apply #'format nil fmt args))))

(defun expand-fail (fmt args fmt1 &rest args1)
  `(fail ,@(if fmt
               #+nil`("~?: ~?" ,fmt1 (list ,@args1) ,fmt (list ,@args))
               `(,fmt ,@args)
               `(,fmt1 ,@args1))))

(defun expand-is (test fmt args &optional neg-p)
  (flet ((proper-length-p (n)
           (and (proper-list-p test)
                (length= n test)))
         (values-p (x)
           (and (proper-list-p x)
                (eq 'values (first x))))
         (constant-value-p (x)
           (or (numberp x)
               (stringp x)
               (keywordp x)
               (and (proper-list-p x)
                    (eq 'quote (first x))))))
    (cond ((and (proper-length-p 2)
                (eq 'not (first test)))
           (expand-is (second test) fmt args (not neg-p)))
          ((proper-length-p 2)
           (destructuring-bind (check value-form) test
             (with-gensyms (v)
               `(let ((,v ,value-form))
                  ,(if neg-p
                       `(when (,check ,v)
                          ,(expand-fail fmt args
                                        "~s evaluated to ~s~%which satisfies ~s while it shoudln't"
                                        `(quote ,value-form) v `(quote ,check)))
                       `(unless (,check ,v)
                          ,(expand-fail fmt args
                                        "~s evaluated to ~s~%which doesn't satisfy ~s"
                                        `(quote ,value-form) v `(quote ,check))))))))
          ((proper-length-p 3)
           (destructuring-bind (pred expected actual) test
             (with-gensyms (exp act)
               `(let ,(if (values-p expected)
                          `((,exp (list ,@(rest expected)))
                            (,act (multiple-value-list ,actual)))
                          `((,exp ,expected)
                            (,act ,actual)))
                  ,(if neg-p
                       `(when (,pred ,exp ,act)
                          ,(if (constant-value-p expected)
                               (expand-fail fmt args
                                            "~s evaluated to~%~s~%which is ~s to~%~s while it shouldn't"
                                            `(quote ,actual) act `(quote ,pred) exp)
                               (expand-fail fmt args
                                            "~s evaluated to~%~s~%which is ~s to~%~s =~%~s while it shouldn't"
                                            `(quote ,actual) act `(quote ,pred)
                                            `(quote ,expected) exp)))
                       `(unless (,pred ,exp ,act)
                          ,(if (constant-value-p expected)
                               (expand-fail fmt args
                                            "~s evaluated to~%~s~%which isn't ~s to~%~s"
                                            `(quote ,actual) act `(quote ,pred) exp)
                               (expand-fail fmt args
                                            "~s evaluated to~%~s~%which isn't ~s to~%~s =~%~s"
                                            `(quote ,actual) act `(quote ,pred)
                                            `(quote ,expected) exp))))))))
          (t
           `(is-true ,test ,fmt ,@args)))))

(defmacro is (test &optional fmt &rest args)
  (expand-is test fmt args))

(defmacro is-true (expr &optional fmt &rest args)
  (with-gensyms (v)
    `(let ((,v ,expr))
       (unless ,v
         ,(expand-fail fmt args
                       "~s evaluated to ~s which isn't true"
                       `(quote ,expr) v)))))

(defmacro is-false (expr &optional fmt &rest args)
  (with-gensyms (v)
    `(let ((,v ,expr))
       (when ,v
         ,(expand-fail fmt args
                       "~s evaluated to ~s which isn't false"
                       `(quote ,expr) v)))))

(defmacro signals (spec &body body)
  (destructuring-bind (condition &optional fmt &rest args) (ensure-list spec)
    `(handler-case (progn
                     (block nil ,@body)
                     ,(expand-fail fmt args
                                   "expected condition ~s not signalled"
                                   `(quote ,condition)))
       (,condition () nil))))

;;; LOGGED-FIXTURE

(defvar *verbose-test-logging* nil)

(defclass logged-fixture ()
  ((log :accessor log-of :initform '())))

(defun format-list (l)
  (let ((p *package*))
    (loop for item in l
          collect (with-output-to-string (out)
                    (with-standard-io-syntax
                        (let ((*package* p)
                              (*print-readably* nil))
                          (write item :stream out)))))))

(defun diff (expected actual)
  (with-output-to-string (out)
    (difflib:unified-diff
     out (format-list expected) (format-list actual)
     :test-function #'equal
     :from-file "expected" :to-file "actual")))

(defmacro expecting (&body body)
  (let* ((p (or (position '==> body)
                (error "==> not found")))
         (actual-body (subseq body 0 p))
         (expected-in (subseq body (1+ p))))
    (with-gensyms (log-start actual expected)
      `(progn
         (let ((,log-start (log-of *fixture*)))
           (progn ,@actual-body)
           (let ((,actual (reverse (ldiff (log-of *fixture*) ,log-start)))
                 (,expected (list ,@(loop for item in expected-in
                                          collect (if (atom item)
                                                      item
                                                      `(list ,@item))))))
             (is (equal ,expected ,actual)
                 "log mismatch:~%~a"
                 (diff ,expected ,actual))))))))

(defun << (&rest things)
  (dolist (item things)
    (push item (log-of *fixture*))
    (when *verbose-test-logging*
      (format *debug-io* "~&<< ~s~%" item))))

(defmacro <<< (&rest things)
  (flet ((expand (thing)
           (if (or (stringp thing) (keywordp thing))
               thing
               `(list ',thing := ,thing))))
    `(<< ,@(mapcar #'expand things))))
