(in-package :vtf)

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
                (eq 'values (first x)))))
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
                                        "~s evaluated to ~s which satisfies ~s while it shoudln't"
                                        `(quote ,value-form) v `(quote ,check)))
                       `(unless (,check ,v)
                          ,(expand-fail fmt args
                                        "~s evaluated to ~s which doesn't satisfy ~s"
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
                          ,(expand-fail fmt args
                                        "~s evaluated to~%~s which is ~s to ~s =~%~s while it shouldn't"
                                        `(quote ,actual) act `(quote ,pred)
                                        `(quote ,expected) exp))
                       `(unless (,pred ,exp ,act)
                          ,(expand-fail fmt args
                                        "~s evaluated to~%~s which isn't ~s to ~s =~%~s"
                                        `(quote ,actual) act `(quote ,pred)
                                        `(quote ,expected) exp)))))))
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

;;; PROTOCOL

(defgeneric test-items (object))

(defgeneric test-item-name (object)
  (:method ((object t)) nil))

(defgeneric run-tests (object)
  (:method (object)
    (let ((n-pass 0)
          (failed '()))
      (dolist (item (test-items object))
        (setup item)
        (unwind-protect
             (multiple-value-bind (np f)
                 (run-tests item)
               (incf n-pass np)
               (appendf failed f))
          (teardown item)))
      (format t "~&---~%*** TOTAL~@[ IN ~A~]: ~s tests, ~s passed, ~s failed~@[: ~s~]~%"
              (test-item-name object)
              (+ n-pass (length failed))
              n-pass (length failed) failed)
      (values n-pass failed))))

(defgeneric setup (fixture)
  (:method ((fixture t)) nil))

(defgeneric teardown (fixture)
  (:method ((fixture t)) nil))

;;; TEST CASE

(defclass test-case ()
  ((name :accessor name :initarg :name)
   (test-function :accessor test-function :initarg :test-function)))

(defmethod run-tests ((object test-case))
  (handler-case
      (progn
        (funcall (test-function object))
        (format t "~&*** PASS: ~s~%" (name object))
        (values 1 '()))
    (serious-condition (c)
      (format t "~&*** FAIL: ~s~%~s: ~a~%---~%" (name object) (type-of c) c)
      (values 0 (list (name object))))))

;;; FIXTURES

(defvar *package-fixtures* (make-hash-table))
(defvar *fixture-test-cases* (make-hash-table))
(defvar *fixture* nil)
(defvar *last-fixture* nil)

(defmethod test-item-name ((fixture standard-object))
  (format nil "FIXTURE ~s" (type-of fixture)))

(defmethod test-items ((fixture standard-object))
  (reverse (gethash (type-of fixture) *fixture-test-cases*)))

(defmethod run-tests ((fixture standard-object))
  (let ((*fixture* (setf *last-fixture* fixture)))
    (call-next-method)))

(defmacro define-fixture (name direct-superclasses direct-slots &rest options)
  `(progn
     (defclass ,name ,direct-superclasses ,direct-slots
       ,@(remove :abstract options :key #'first))
     (setf (get ',name 'fixture-p) t)
     ,@(unless (find :abstract options :key #'first)
         `((pushnew ',name (gethash (symbol-package ',name) *package-fixtures*))))))

;;; PACKAGES

(defvar *package-test-cases* (make-hash-table))

(defmethod test-item-name ((package package))
  (format nil "PACKAGE ~s" (package-name package)))

(defmethod test-items ((package package))
  (append (mapcar #'make-instance
                  (reverse (gethash (find-package package) *package-fixtures*)))
          (reverse (gethash (find-package package) *package-test-cases*))))

;;; CONVENIENCE

(defmethod test-items ((object (eql t)))
  (sort (union (hash-table-keys *package-test-cases*)
               (hash-table-keys *package-fixtures*))
        #'string< :key #'package-name))

(defmethod run-tests ((name symbol))
  (cond ((eq t name)
         (call-next-method))
        ((get name 'test-case)
         (run-tests (get name 'test-case)))
        ((get name 'fixture-p)
         (run-tests (make-instance name)))
        ((find-package name)
         (run-tests (find-package name)))
        (t
         (error "cannot locate any tests for the specified name: ~s" name))))

(defun run-all-tests ()
  (run-tests t))

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
           body)))

(defmacro deftest (name slots (&optional fixture-spec &rest options) &body body)
  (declare (ignore options))
  (assert (or (not (null fixture-spec))
              (null slots))
          () "cannot specify slots without fixture")
  (assert (or (null fixture-spec)
              (typep fixture-spec '(or symbol
                                    (cons symbol (cons symbol nil)))))
          () "invalid fixture spec ~s" fixture-spec)
  (multiple-value-bind (fixture-var fixture-name)
      (etypecase fixture-spec
        (null (values (gensym) nil))
        (symbol (values (gensym) fixture-spec))
        (t (values (first fixture-spec) (second fixture-spec))))
    `(progn
       (defun ,name (&optional ,(if fixture-name
                                    `(,fixture-var (setup-fixture ',fixture-name))
                                    fixture-var))
         (with-fixture ,slots ,fixture-var ,@body))
       (pushnew ',name
                ,(if fixture-name
                     `(gethash ',fixture-name *fixture-test-cases*)
                     `(gethash (symbol-package ',name) *package-test-cases*)))
       (setf (get ',name 'test-case)
             (make-instance 'test-case
                            :name ',name
                            :test-function ',name)))))

;;; 5AM COMPAT (to be removed)

(defmacro in-suite (name)
  (declare (ignore name))
  #+nil (simple-style-warning "IN-SUITE is retained only for 5am compatibility")
  nil)

(defmacro def-suite (&rest args)
  (declare (ignore args))
  #+nil (simple-style-warning "DEF-SUITE is retained only for 5am compatibility")
  nil)

;;; LOGGED-FIXTURE & related

(defvar *verbose-test-logging* nil)

(defclass logged-fixture ()
  ((log :accessor log-of :initform '())))

(defun format-list (l)
  (let ((p *package*))
    (loop for item in l
          collect (with-output-to-string (out)
                    (with-standard-io-syntax
                        (let ((*package* p))
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

;; TBD: call setup / teardown inside handler-case for each test!!!