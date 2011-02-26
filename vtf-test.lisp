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

(deftest/self test-simple-pass
  (let ((results (run-tests 'simple-pass)))
    (is (length= 1 results))
    (is (= 1 (vtf::n-passed (first results))))
    (is (null (vtf::failed-names (first results))))))

(deftest/self test-simple-fail
  (let ((results (run-tests 'simple-fail)))
    (is (length= 1 results))
    (is (= 0 (vtf::n-passed (first results))))
    (is (equal '(simple-fail)
               (vtf::failed-names (first results))))))

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
  (let ((results (run-tests 'base-fixture)))
    (is (length= 1 results))
    (is (= 1 (vtf::n-passed (first results))))
    (is (equal '(test-for-base-fixture/fail)
               (vtf::failed-names (first results))))
    (is-true (typep *last-fixture* 'base-fixture))
    (is (equal '(:setup-base-fixture
                 :test-for-base-fixture/fail
                 :teardown-base-fixture
                 :setup-base-fixture
                 :test-for-base-fixture/pass
                 :teardown-base-fixture)
               (reverse
                (seq *last-fixture*))))))

(with-alt-tests
  (define-fixture inherited-fixture (base-fixture)
    ((seq :accessor seq :initform '()))))

(defmethod setup :after ((fixture inherited-fixture))
  (push :setup-inherited-fixture (seq fixture)))

(defmethod run-fixture-test-case ((fixture inherited-fixture) (test-case t) teardown-p)
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
  (let ((results (run-tests 'inherited-fixture)))
    (is (length= 1 results))
    (is (= 2 (vtf::n-passed (first results))))
    (is (equal '(test-for-base-fixture/fail test-for-inherited-fixture/fail)
               (vtf::failed-names (first results))))
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
                (seq *last-fixture*))))))

(deftest/self test-package
  (let ((results (run-tests 'vtf-test)))
    (is (length= 1 results))
    (is (= 4 (vtf::n-passed (first results))))
    ;; base-fixture is executed before simple-fail
    (is (equal '(test-for-base-fixture/fail test-for-base-fixture/fail
                 test-for-inherited-fixture/fail simple-fail)
               (vtf::failed-names (first results))))))

;; TBD: test running all tests
;; TBD: test checks
;; TBD: test expectations
