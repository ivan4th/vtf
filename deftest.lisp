(in-package :vtf)

;;; TEST and DEFTEST macros

(defmacro test (spec &body body)
  "5am compatibility macro"
  #+nil (simple-style-warning "TEST is retained only for 5am compatibility")
  (destructuring-bind (name &key suite)
      (ensure-list spec)
    (declare (ignore suite)) ;; FIXME: 5am compat
    `(deftest ,name () () ,@body)))

(defun setup-fixture (name)
  "Create a fixture of class NAME and run SETUP on it. To be used for
  REPL-based debugging purposes."
  (let ((fixture (make-instance name)))
    (setup fixture)
    fixture))

(defmacro with-fixture (slots expr &body body)
  "Evaluate EXPR to get a fixture and execute body in dynamic context
  in which the fixture is current (i.e. *FIXTURE* is bound to it) and
  lexical context in which variables specified as SLOTS are bound to
  corresponding slots of the fixture."
  `(let ((*fixture* ,expr)) ;; needed for direct invocation
     ,@(if slots
           `((with-slots ,slots *fixture*
               ,@body))
           body)))

(defmacro deftest (name slots (&optional fixture-spec) &body body)
  "Define a test case named NAME. The optional FIXTURE-SPEC must be of
  form FIXTURE-NAME or (FIXTURE-VAR FIXTURE-NAME). If FIXTURE is
  specified, the test is registered as belonging to FIXTURE and
  *FIXTURE* special variable is bound to an instance of fixture class
  for the duration of test execution. If FIXTURE-VAR is specified,
  it's lexically bound to that fixture instance, too.

  The BODY of deftest macro is compiled immediately, i.e. no delayed
  compilation/evaluation is used."
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
             (with-simple-restart (abort "Abort test case")
               (run-test-item (get ',name 'test-case)))
             *last-fixture*))))))
