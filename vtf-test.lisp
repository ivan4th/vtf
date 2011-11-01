(in-package :vtf-test)

(defvar *alt-test-items* '())

(defmacro with-alt-tests (&body body)
  `(let ((vtf::*test-items* *alt-test-items*)
         (vtf::*keep-fixture* nil))
     ,@body
     (setf *alt-test-items* vtf::*test-items*)))

(defmacro deftest/self (name &body body)
  `(deftest ,name () ()
     (with-alt-tests
       (let ((vtf::*test-verbose* nil))
         ,@body))))

(defmacro deftest/alt (name slots fixture-and-options &body body)
  `(with-alt-tests
     (deftest ,name ,slots ,fixture-and-options
       ,@body)))

(deftest/alt simple-pass () () nil)

(deftest/alt simple-fail () () (fail "oops ~d" 42))

(deftest/alt multi-pass () ()
  (check-passed 'some-test)
  (check-passed 'another-test)
  (check-passed "test 3"))

(deftest/alt multi-fail () ()
  (check-passed 'some-test)
  (check-failed 'another-test)
  (check-failed "test 3"))

(defun verify-results (results &rest items)
  (is (= (/ (length items) 2) (length results)))
  (loop for (n-passed failed-names) on items by #'cddr
        for result in results
        do (is (= n-passed (vtf::n-passed result)))
           (is (equal failed-names (vtf::failed-names result)))))

(deftest/self test-simple-pass
  (verify-results (run-tests 'simple-pass) 1 '()))

(deftest/self test-simple-fail
  (verify-results (run-tests 'simple-fail) 0 '(simple-fail)))

(deftest/self test-multi-pass
  (verify-results (run-tests 'multi-pass) 1 '() 1 '() 1 '() 1 '()))

(deftest/self test-multi-fail
  (verify-results (run-tests 'multi-fail)
                  1 '() 0 '(another-test) 0 '("test 3") 0 '(multi-fail)))

(with-alt-tests
  (define-fixture base-fixture ()
    ((seq :accessor seq :initform '()))))

(defmethod setup :after ((fixture base-fixture))
  (push :setup-base-fixture (seq fixture)))

(defmethod teardown :before ((fixture base-fixture))
  (push :teardown-base-fixture (seq fixture)))

(deftest/alt test-for-base-fixture/pass (seq) (base-fixture)
  (push :test-for-base-fixture/pass seq))

(deftest/alt test-for-base-fixture/fail (seq) (base-fixture)
  (push :test-for-base-fixture/fail seq)
  (fail "failed"))

(deftest/self test-plain-fixture
  (verify-results (run-tests 'base-fixture)
                  1 '(test-for-base-fixture/fail))
  (is-true (typep *last-fixture* 'base-fixture))
  (is (equal '(:setup-base-fixture
               :test-for-base-fixture/fail
               :teardown-base-fixture
               :setup-base-fixture
               :test-for-base-fixture/pass
               :teardown-base-fixture)
             (reverse
              (seq *last-fixture*)))))

(with-alt-tests
  (define-fixture inherited-fixture (base-fixture)
    ((seq :accessor seq :initform '()))))

(defmethod setup :after ((fixture inherited-fixture))
  (push :setup-inherited-fixture (seq fixture)))

(defmethod run-fixture-test-case ((fixture inherited-fixture) (test-case t) teardown-p debug-p)
  (push :run-fixture-test-case-in (seq fixture))
  (unwind-protect
       (call-next-method)
    (push :run-fixture-test-case-out (seq fixture))))

(defmethod teardown :before ((fixture inherited-fixture))
  (push :teardown-inherited-fixture (seq fixture)))

(deftest/alt test-for-inherited-fixture/pass (seq) (inherited-fixture)
  (push :test-for-inherited-fixture/pass seq))

(deftest/alt test-for-inherited-fixture/fail (seq) (inherited-fixture)
  (push :test-for-inherited-fixture/fail seq)
  (fail "failed"))

(deftest/self test-inherited-fixture
  (verify-results (run-tests 'inherited-fixture)
                  2 '(test-for-base-fixture/fail test-for-inherited-fixture/fail))
  (is-true (typep *last-fixture* 'inherited-fixture))
  (is (equal '(:run-fixture-test-case-in
               :setup-base-fixture
               :setup-inherited-fixture
               :test-for-base-fixture/fail
               :teardown-inherited-fixture
               :teardown-base-fixture
               :run-fixture-test-case-out

               :run-fixture-test-case-in
               :setup-base-fixture
               :setup-inherited-fixture
               :test-for-base-fixture/pass
               :teardown-inherited-fixture
               :teardown-base-fixture
               :run-fixture-test-case-out

               :run-fixture-test-case-in
               :setup-base-fixture
               :setup-inherited-fixture
               :test-for-inherited-fixture/fail
               :teardown-inherited-fixture
               :teardown-base-fixture
               :run-fixture-test-case-out

               :run-fixture-test-case-in
               :setup-base-fixture
               :setup-inherited-fixture
               :test-for-inherited-fixture/pass
               :teardown-inherited-fixture
               :teardown-base-fixture
               :run-fixture-test-case-out)
             (reverse
              (seq *last-fixture*)))))

(deftest/self test-package
  ;; base-fixture (and then multi-diff-fixture) is executed before
  ;; multi-fail / simple-fail. ABT fixtures fail because of absent
  ;; data files
  (verify-results (run-tests 'vtf-test)
                  9 '(test-for-base-fixture/fail
                      test-for-base-fixture/fail
                      test-for-inherited-fixture/fail
                      some-data "more-data" "extra" multi-diff
                      another-test "test 3" multi-fail
                      possibly-failing-single-diff single-diff-1
                      single-diff-2 simple-fail
                      something more-xml-stuff xml-diff)))

(defvar *mangle-data* nil)

(with-alt-tests
  (define-fixture multi-diff-fixture (abt-lisp-output-mixin) ()))

(deftest/alt multi-diff () (multi-diff-fixture)
  (with-abt-section (#p"/abc/def/")
    (abt-emit '((:abc 123) (:def 456)) 'some-data)
    (cond ((not *mangle-data*)
           (abt-emit '((:qqq 555)) "more-data")
           (abt-emit '(:zzz) "extra"))
          (t
           (abt-emit '((:qqq 666)) "more-data")
           (abt-emit '(:zzz) "eprst")))))

(defvar *fake-abt-data*)

(defun invoke-with-fake-abt-data (thunk)
  (flet ((validate-path (path)
           (is (pathnamep path))
           (is (equal '(:absolute "abc" "def") (pathname-directory path)))))
    (let ((*fake-abt-data* (make-hash-table :test #'equal))
          (vtf::*abt-read-function*
           #'(lambda (path)
               (validate-path path)
               (values (gethash (file-namestring path) *fake-abt-data*))))
          (vtf::*abt-write-function*
           #'(lambda (data path)
               (validate-path path)
               (setf (gethash (file-namestring path) *fake-abt-data*)
                     data)))
          (vtf::*abt-del-function*
           #'(lambda (path)
               (validate-path path)
               (is-true (nth-value 1 (gethash (file-namestring path) *fake-abt-data*)))
               (remhash (file-namestring path) *fake-abt-data*)))
          (vtf::*abt-dir-function*
           #'(lambda (path)
               (validate-path path)
               (is (null (pathname-name path)))
               (is (null (pathname-type path)))
               (hash-table-keys *fake-abt-data*)))
          (*mangle-data* nil))
      (funcall thunk))))

(defmacro with-fake-abt-data (&body body)
  `(invoke-with-fake-abt-data #'(lambda () ,@body)))

(deftest/self test-abt-multi-diff
  (with-fake-abt-data
    (verify-results (run-tests 'multi-diff-fixture)
                    0 '(some-data "more-data" "extra" multi-diff))
    (abt-accept '(some-data "more-data"))
    (verify-results (run-tests 'multi-diff-fixture)
                    2 '("extra" multi-diff))
    (abt-accept)
    (verify-results (run-tests 'multi-diff-fixture) 4 '())
    (setf *mangle-data* t)
    (verify-results (run-tests 'multi-diff-fixture)
                    1 '("more-data" "eprst" multi-diff))
    (abt-accept '("more-data" "eprst"))
    (verify-results (run-tests 'multi-diff-fixture)
                    3 '(multi-diff)) ;; one missing item remains
    (abt-accept)
    (verify-results (run-tests 'multi-diff-fixture) 4 '())))

(with-alt-tests
  (define-fixture sample-abt-fixture (abt-fixture)
    ()
    (:default-initargs :data-location #p"/abc/def/")))

(deftest/alt single-diff-1 () (sample-abt-fixture)
  (<< 'abc)
  (if *mangle-data*
      (<<< (+ 10 20))
      (<<< (+ 1 2))))

(deftest/alt single-diff-2 () (sample-abt-fixture)
  (<< 'qqq 'rrr))

(deftest/alt possibly-failing-single-diff () (sample-abt-fixture)
  (<< 'zzz)
  (when *mangle-data*
    (fail "cannot mangle")))

(deftest/self test-abt-single-diff
  (with-fake-abt-data
    (verify-results (run-tests 'sample-abt-fixture)
                    0 '(possibly-failing-single-diff single-diff-1 single-diff-2))
    (abt-accept '(possibly-failing-single-diff single-diff-1))
    (verify-results (run-tests 'sample-abt-fixture)
                    2 '(single-diff-2))
    (abt-accept)
    (verify-results (run-tests 'sample-abt-fixture) 3 '())
    (setf *mangle-data* t)
    (verify-results (run-tests 'sample-abt-fixture)
                    1 '(possibly-failing-single-diff single-diff-1))
    (abt-accept)
    (verify-results (run-tests 'sample-abt-fixture)
                    2 '(possibly-failing-single-diff))))

(define-fixture sample-abt-fixture/real-fs (abt-fixture)
  ()
  (:default-initargs :data-location '(:asdf vtf-test)))

(deftest sample-diff/real-fs () (sample-abt-fixture/real-fs)
  (<< "some stuff goes here" 123)
  (let ((x 5)
        (y 42))
    (<<< x y (+ x y))))

(with-alt-tests
  (define-fixture xml-diff-fixture (vtf-xml:abt-xml-output-mixin) ()))

(deftest/alt xml-diff () (xml-diff-fixture)
  (with-abt-section (#p"/abc/def/")
    (abt-emit (if *mangle-data*
                  "<a><x z='4'/></a>"
                  "<a><x z='3'/></a>")
              'something)
    (abt-emit (cxml:parse-rod "<a f='1'/>" (stp:make-builder)) 'more-xml-stuff)))

(deftest/self test-abt-xml-diff
  (with-fake-abt-data
    (verify-results (run-tests 'xml-diff-fixture)
                    0 '(something more-xml-stuff xml-diff))
    (abt-accept)
    (verify-results (run-tests 'xml-diff-fixture) 3 '())
    (setf *mangle-data* t)
    (verify-results (run-tests 'xml-diff-fixture)
                    1 '(something xml-diff))
    (abt-accept)
    (verify-results (run-tests 'xml-diff-fixture) 3 '())))

;; TBD: test running all tests
;; TBD: test checks
;; TBD: test expectations
