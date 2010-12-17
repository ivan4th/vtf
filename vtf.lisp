(in-package :vtf)

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
	(*print-length* 10))
    (error 'test-failure :message (apply #'format nil fmt args))))

(defun expand-fail (fmt args fmt1 &rest args1)
  `(fail ,@(if fmt
               `("~?: ~?" ,fmt1 (list ,@args1) ,fmt (list ,@args))
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
                                        "~s evaluated to ~s which is ~s to ~s = ~s while it shouldn't"
                                        `(quote ,actual) act `(quote ,pred)
                                        `(quote ,expected) exp))
                       `(unless (,pred ,exp ,act)
                          ,(expand-fail fmt args
                                        "~s evaluated to ~s which isn't ~s to ~s = ~s"
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
  (destructuring-bind (condition fmt &rest args) (ensure-list spec)
    `(handler-case (progn
                     (block nil ,@body)
                     ,(expand-fail fmt args
                                   "expected condition ~s not signalled"
                                   `(quote ,condition)))
       (,condition () nil))))

(defmacro in-suite (name)
  (declare (ignore name))
  #+nil (simple-style-warning "IN-SUITE is retained only for 5am compatibility")
  nil)

(defmacro def-suite (&rest args)
  (declare (ignore args))
  #+nil (simple-style-warning "DEF-SUITE is retained only for 5am compatibility")
  nil)

(defvar *package-test-cases* (make-hash-table))

(defmacro test (spec &body body)
  #+nil (simple-style-warning "TEST is retained only for 5am compatibility")
  (let ((name (ensure-car spec)))
    `(progn
       (defun ,name ()
         ,@body)
       (pushnew ',name (gethash *package* *package-test-cases*)))))

(defun run-tests (&optional (package *package*))
  (let ((n-pass 0)
        (failed '()))
    (dolist (test (reverse (gethash (find-package package) *package-test-cases*)))
      (handler-case
          (progn
            (funcall test)
            (incf n-pass)
            (format t "~&PASS: ~s~%" test))
        (serious-condition (c)
          (push test failed)
          (format t "~&FAIL: ~s~%~s: ~a~%---~%" test (type-of c) c))))
    (format t "~&---~%TOTAL IN PACKAGE ~s: ~s tests, ~s passed, ~s failed~@[: ~s~]~%"
            (package-name package) (+ n-pass (length failed))
            n-pass (length failed) failed)
    (values n-pass failed)))

(defun run-all-tests ()
  (let ((n-pass 0)
        (failed '()))
    (dolist (package (sort (mapcar #'package-name
                                   (hash-table-keys *package-test-cases*))
                           #'string<))
      (multiple-value-bind (np f)
          (run-tests package)
        (incf n-pass np)
        (appendf failed f)))
    (format t "~&---~%TOTAL: ~s tests, ~s passed, ~s failed~@[: ~s~]~%"
            (+ n-pass (length failed))
            n-pass (length failed) failed)
    (values n-pass failed)))
