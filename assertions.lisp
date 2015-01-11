(in-package :vtf)

;;; ASSERTIONS

(defparameter *min-diff-length* 30
  "Minimum number of characters in expected and actual
  values for which unified diff is shown")

(define-condition test-failure (simple-error)
  ()
  (:documentation "Base condition for assertion failures."))

(defun maybe-diff-values (expected actual)
  "Produce unified diff between pretty-printed representation of
   EXPECTED and ACTUAL if they're big enough for diff to make sense,
   return NIL otherwise"
  (flet ((format-value (v)
	   (let ((p *package*))
	     (with-standard-io-syntax
	       (let ((*package* p)
		     (*print-right-margin* 80)
		     (*print-miser-width* 70)
		     (*print-pretty* t)
		     (*print-readably* nil))
		 (split-sequence:split-sequence
		  #\newline
		  (prin1-to-string v)))))))
    (let ((exp (format-value expected))
	  (act (format-value actual)))
      (when (or (> (reduce #'+ (mapcar #'length exp)) *min-diff-length*)
		(> (reduce #'+ (mapcar #'length act)) *min-diff-length*))
	(with-output-to-string (out)
	  (difflib:unified-diff
	   out exp act
	   :test-function #'equal
	   :from-file "expected" :to-file "actual"))))))

(defun fail (fmt &rest args)
  "Signal a test failure. Stops execution of the test case."
  (let ((*print-level* 4)               ; FIXME - extract constants
        (*print-length* 100))
    (error 'test-failure
           :format-control "Test failure signalled:~%~?"
           :format-arguments (list fmt args))))

(defun expand-fail (fmt args fmt1 &rest args1)
  `(fail ,@(if fmt
               #++`("~?: ~?" ,fmt1 (list ,@args1) ,fmt (list ,@args))
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
                                            "~s evaluated to~%~s~%which is ~s to ~s~%while it shouldn't"
                                            `(quote ,actual) act `(quote ,pred) exp)
                               (expand-fail fmt args
                                            "~s evaluated to~%~s~%which is ~s to ~s =~%~s~%while it shouldn't"
                                            `(quote ,actual) act `(quote ,pred)
                                            `(quote ,expected) exp)))
                       `(unless (,pred ,exp ,act)
                          ,(if (constant-value-p expected)
                               (expand-fail fmt args
                                            "~s evaluated to~%~s~%which isn't ~s to~%~s~@[~%DIFF:~%~a~]"
                                            `(quote ,actual) act `(quote ,pred) exp
					    `(maybe-diff-values ,exp ,act))
                               (expand-fail fmt args
                                            "~s evaluated to~%~s~%which isn't ~s to~%~s =~%~s~@[~%DIFF:~%~a~]"
                                            `(quote ,actual) act `(quote ,pred)
                                            `(quote ,expected) exp
					    `(maybe-diff-values ,exp ,act)))))))))
          (t
           `(is-true ,test ,fmt ,@args)))))

(defmacro is (test &optional fmt &rest args)
  "DWIM test form inspired by 5am test framework. The TEST must be
  either of the form (predicate expected actual) or (predicate actual).
  FMT and ARGS may be used to specify additional message for the
  case of check failure. FMT and ARGS aren't evaluated unless the
  check fails."
  (expand-is test fmt args))

(defmacro is-true (expr &optional fmt &rest args)
  "Check that EXPR evaluates to true.
  FMT and ARGS may be used to specify additional message for the case
  of check failure. FMT and ARGS aren't evaluated unless the check
  fails."
  (with-gensyms (v)
    `(let ((,v ,expr))
       (unless ,v
         ,(expand-fail fmt args
                       "~s evaluated to ~s which isn't true"
                       `(quote ,expr) v)))))

(defmacro is-false (expr &optional fmt &rest args)
  "Check that EXPR evaluates to false.
  FMT and ARGS may be used to specify additional message for the case
  of check failure. FMT and ARGS aren't evaluated unless the check
  fails."
  (with-gensyms (v)
    `(let ((,v ,expr))
       (when ,v
         ,(expand-fail fmt args
                       "~s evaluated to ~s which isn't false"
                       `(quote ,expr) v)))))

(defmacro signals (spec &body body)
  "Check that evaluation of BODY causes the specified condition
  to be signalled. SPEC can be either of form CONDITION or
  (CONDITION [FMAT [ARGS]]). FMT and ARGS may be used to specify
  additional message for the case of check failure. FMT and ARGS aren't
  evaluated unless the check fails."
  (destructuring-bind (condition &optional fmt &rest args) (ensure-list spec)
    `(handler-case (progn
                     (block nil ,@body)
                     ,(expand-fail fmt args
                                   "expected condition ~s not signalled"
                                   `(quote ,condition)))
       (,condition () nil))))
