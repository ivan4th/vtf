(in-package :vtf)

(defvar *test-items* '()
  "The list of all available test cases")
(defvar *test-verbose* t
  "Set to true to make VTF display the result of each completed test case")
(defvar *current-test-case* nil
  "Currently executing test case")
(defvar *fixture* nil
  "Currently executing fixture")
(defvar *last-fixture* nil
  "Last fixture")
(defvar *keep-fixture* nil
  "Set to true to skip fixture teardown")
(defvar *redirect-test-output* t
  "Set to true to only show *standard-output* of failed test cases")

;;; PROTOCOL

(defgeneric run-test-item (item)
  (:documentation "Executes all the tests corresponding to ITEM (see methods)."))

(defgeneric setup (fixture)
  (:documentation "Prepare FIXTURE for running a test case. Invoked once before each
  test case."))

(defgeneric teardown (fixture)
  (:documentation "Perform cleanup for FIXTURE after running a test case. Invoked
  once after each test case."))

(defgeneric invoke-test-case (fixture test-case)
  (:documentation "Run the specified TEST-CASE using FIXTURE. Invoked between
  SETUP and TEARDOWN. Can be used to set up a dynamic environment for the test
  case."))

(defgeneric run-fixture-test-case (fixture test-case teardown-p debug-p)
  (:documentation "Run the specified TEST-CASE using FIXTURE. Perform
  fixture teardown if TEARDOWN-P is true. Handle any unhandled SERIOUS-CONDITIONs
  unless DEBUG-P is true.

  The default implementation performs SETUP - INVOKE-TEST-CASE -
  (optional)TEARDOWN sequence and registers test success or failure.
  An :around method can be used to set up a dynamic environment that must
  affect both test case itself and SETUP/TEARDOWN methods."))

(defgeneric n-passed (result)
  (:documentation "Return a number of successful test cases recorded in RESULT"))

(defgeneric failed-names (result)
  (:documentation "Return the names of failed test cases recorded in RESULT"))

(defgeneric display-result (result stream)
  (:documentation "Print test RESULT to the STREAM"))

(defgeneric aggregate-title (result)
  (:documentation "Return the name of an aggregate RESULT if applicable"))

(defgeneric condition-result (condition)
  (:documentation "Derive test result from CONDITION"))

(defgeneric fixture-names (fixture)
  (:method-combination append)
  (:documentation "Return a list of fixture names that correspond to test
  cases that belong to FIXTURE. Besides the name of FIXTURE itself this
  includes all fixture classes from which FIXTURE's class is inherited."))

;;; TEST RESULT

(defclass test-result ()
  ((name :reader name
         :initarg :name
         :type (or string symbol null)
         :initform (error "must specify the name of the test result")
         :documentation "The name of an item that produced this result"))
  (:documentation "Base class for test results"))

(defclass single-check-result (test-result)
  ()
  (:documentation "Result of a single test case"))

(defclass single-check-success (single-check-result)
  ()
  (:documentation "Denotes the result of a successful test case"))

(defmethod n-passed ((result single-check-success)) 1)

(defmethod failed-names ((result single-check-success)) '())

(defmethod display-result ((result single-check-success) stream)
  (format stream "~&*** PASS: ~s~%" (name result)))

(defclass single-check-failure (single-check-result)
  ((kind :reader kind
         :initarg :kind
         :type keyword
         :initform :fail
         :documentation "The type of test failure. One of :FAIL (simple test failure),
         :DIFF (diff of an ABT output item) or :NEW (new ABT output item)")
   (cause :reader cause
          :initarg :cause
          :type (or condition string null)
          :initform nil
          :documentation "A condition or a string describing the cause of test failure"))
  (:documentation "Denotes the result of a failed test case"))

(defmethod n-passed ((result single-check-failure)) 0)

(defmethod failed-names ((result single-check-failure))
  (list (name result)))

(defmethod display-result ((result single-check-failure) stream)
  (cond ((not (cause result))
         (format stream "~&*** ~a: ~s~%" (kind result) (name result)))
        ((typep (cause result) 'condition)
         (format stream "~&*** ~a: ~s~%~s: ~a~%---~%"
                 (kind result) (name result) (type-of (cause result))
                 (cause result)))
        (t
         (format stream "~&*** ~a: ~s~%~a~%---~%"
                 (kind result) (name result) (cause result)))))

(defclass aggregate-result (test-result)
  ((children :accessor children :initarg :children
             :type list
             :initform '()
             :documentation "List of child results"))
  (:documentation "Denotes the aggregate result of several test cases"))

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

(defclass fixture-result (aggregate-result) ()
  (:documentation "Denotes the result of test cases belonging to a fixture"))

(defmethod aggregate-title ((result fixture-result))
  (format nil "fixture ~a" (name result)))

(defclass package-result (aggregate-result) ()
  (:documentation "Denotes the result of test cases belonging to a package"))

(defmethod aggregate-title ((result package-result))
  (format nil "package ~a" (name result)))

(defclass global-result (aggregate-result) ()
  (:documentation "Denotes the result of all available test cases"))

(defmethod aggregate-title ((result global-result)) nil)

;;; CHECKS

(define-condition check-condition ()
  ((handled-p :accessor handled-p :initform nil)
   (name :reader name :initarg :name
         :initform (error "must specify check condition name")))
  (:documentation "Base condition type for conditions that are
  used to describe the result of test case execution"))

(define-condition check-passed (check-condition) ()
  (:documentation "Condition used to signal successful test case completion"))

(defmethod condition-result ((condition check-passed))
  (make-instance 'single-check-success
                 :name (name condition)))

(define-condition check-failed (check-condition)
  ((kind :reader kind
         :initarg :kind
         :type keyword
         :initform :fail)
   (cause :reader cause
          :initarg :cause
          :type (or condition string null)
          :initform nil))
  (:documentation "Condition used to signal failed test"))

(defmethod condition-result ((condition check-failed))
  (make-instance 'single-check-failure
                 :name (name condition)
                 :kind (kind condition)
                 :cause (cause condition)))

(defun check-passed (name)
  (signal 'check-passed :name name))

(defun check-failed (name &optional (kind :fail) cause)
  (signal 'check-failed :name name :kind kind :cause cause))

;;; TEST ITEM

(defclass test-item ()
  ((name :accessor name :type symbol :initarg :name
         :initform (error "must specify test item name")
         :documentation "The name of the test item")
   (item-package :accessor item-package :type (or package null)
                 :initarg :item-package
                 :initform (error "must specify the package")
                 :documentation "The package this test item belongs to"))
  (:documentation "An item that can be executed by RUN-TEST-ITEM"))

(defmethod print-object ((object test-item) stream)
  (print-unreadable-object (object stream :type t)
    (prin1 (name object) stream)))

;;; TEST CASE

(defclass test-case (test-item)
  ((fixture-name :accessor fixture-name :type symbol :initarg :fixture-name
                 :initform (error "must specify fixture name")
                 :documentation "The name of fixture this test case belongs to, or NIL")
   (test-function :accessor test-function :type symbol :initarg :test-function
                  :initform (error "must specify test function")
                  :documentation "The function that contains the code of this test case"))
  (:documentation "A test case defined by DEFTEST"))

(defmethod print-object ((test-case test-case) stream)
  (print-unreadable-object (test-case stream :type t)
    (prin1 (name test-case) stream)))

(defmethod invoke-test-case ((fixture t) (test-case test-case))
  (funcall (test-function test-case) fixture))

(defmethod run-fixture-test-case ((fixture t) (test-case test-case) teardown-p debug-p)
  (let ((*current-test-case* test-case)
        (pass-p t)
        (need-to-signal t))
    (flet ((run ()
             (handler-bind ((check-failed
                             #'(lambda (c)
                                 (unless (handled-p c)
                                   (setf pass-p nil)
                                   (when (eq (name test-case) (name c))
                                     (setf need-to-signal nil)))))
                            (check-passed
                             #'(lambda (c)
                                 (unless (handled-p c)
                                   (when (eq (name test-case) (name c))
                                     (setf need-to-signal nil))))))
               (setup fixture)
               (unwind-protect
                    (invoke-test-case fixture test-case)
                 (when teardown-p
                   (teardown fixture))))))
      (if debug-p
          (run)
          (let ((out (make-string-output-stream)))
            (flet ((maybe-show-output ()
                     (let ((output (get-output-stream-string out)))
                       (unless (emptyp output)
                         (write-string output *debug-io*)
                         (fresh-line *debug-io*)
                         (terpri *debug-io*)))))
              (handler-case
                  (progn
                    (if *redirect-test-output*
                        (let* ((*debug-io* (make-two-way-stream *debug-io* out))
                               (*standard-output* out))
                          (run))
                        (run))
                    (when need-to-signal
                      (unless pass-p
                        (maybe-show-output))
                      (signal (if pass-p 'check-passed 'check-failed)
                              :name (name test-case))))
                (error (condition)
                  (maybe-show-output)
                  (signal 'check-failed
                          :name (name test-case)
                          :cause condition)))))))))

(defmethod run-test-item ((test-case test-case))
  "Execute the single TEST-CASE"
  (let ((check-results '())
        (io *debug-io*))
    (handler-bind ((check-condition
                    #'(lambda (c)
                        (unless (handled-p c)
                          (setf (handled-p c) t)
                          (let ((result (condition-result c)))
                            (when *test-verbose*
                              (display-result result io))
                            (push result check-results))))))
      (let ((*fixture* (or *fixture*
                           (when (fixture-name test-case)
                             (make-instance (fixture-name test-case))))))
        (setf *last-fixture* *fixture*)
        (run-fixture-test-case *fixture* test-case (not *keep-fixture*)
                               *keep-fixture*)))
    (nreverse check-results)))

;;; TEST LISTS

(defun run-children (result-class name items)
  "Run a list of child test ITEMS of a test item called NAME
  producing a result of RESULT-CLASS"
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

(defclass fixture-test-list (test-item) ()
  (:documentation "Denotes the set of available test cases for a fixture"))

(defmethod run-test-item ((test-list fixture-test-list))
  "Execute test cases belonging to a fixture"
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
  "Define a fixture. NAME, DIRECT-SUPERCLASSES, DIRECT-SLOTS and OPTIONS
  are handled in a manner similar to DEFCLASS. Addidional (:abstract t)
  option is supported which tells VTF that the fixture is only used as
  base class for other fixtures and its test cases shouldn't be run
  directly."
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

(defmethod setup ((fixture t))
  "The default SETUP method does nothing."
  (values))

(defmethod teardown ((fixture t))
  "The default TEARDOWN method does nothing."
  (values))

;;; PACKAGE

(defmethod run-test-item ((package package))
  "Execute test cases defined in PACKAGE"
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
  "Execute all available test cases"
  (run-children 'global-result nil
                (sort
                 (remove-duplicates
                  (loop for (item-type name) in *test-items*
                        for item = (get name item-type)
                        when (item-package item)
                          collect (item-package item)))
                 #'string<
                 :key #'package-name)))
