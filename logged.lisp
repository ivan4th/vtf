(in-package :vtf)

;;; LOGGED-FIXTURE

(defvar *verbose-test-logging* nil
  "Set to true to display items logged via << and <<<")

(defclass logged-fixture ()
  ((log :accessor log-of :initform '()
        :documentation "Log items added so far, in reverse order"))
  (:documentation "Base class for logged fixtures supporting
  EXPECTING, << and <<< operations"))

(defun format-list (l)
  (let ((p *package*))
    (loop for item in l
          collect (with-output-to-string (out)
                    (with-standard-io-syntax
                      (let ((*package* p)
                            (*print-readably* nil)
                            (*print-circle* nil)
                            (*print-case* :downcase))
                        (write item :stream out)))))))

(defmethod setup :after ((fixture logged-fixture))
  "Clear the stored log"
  (setf (log-of fixture) '()))

(defun diff (expected actual)
  "Produce a string representation of diff between expected and actual data"
  (with-output-to-string (out)
    (difflib:unified-diff
     out (format-list expected) (format-list actual)
     :test-function #'equal
     :from-file "expected" :to-file "actual")))

(defun expand-expecting-1 (actual-body expected-in)
  (with-gensyms (log-start actual expected)
    (let* ((relaxed-set-p nil)
           (use-set-p
             (when (and (length= 1 expected-in)
                        (proper-list-p (first expected-in))
                        (case (caar expected-in)
                          (:set t)
                          (:set*
                           (setf relaxed-set-p t)
                           t)))
               (setf expected-in (rest (first expected-in)))
               t)))
      `(let ((,log-start (log-of *fixture*)))
         (progn ,@actual-body)
         (let ((,actual (reverse (ldiff (log-of *fixture*) ,log-start)))
               (,expected (list ,@(loop for item in expected-in
                                        collect (if (atom item)
                                                    item
                                                    `(list ,@item))))))
           ,@(cond (use-set-p
                    (append
                     `((is (set-equal ,expected ,actual :test #'equal)
                           "log mismatch:~%~
                          ~{~s~%~}-->~%~
                          ~@[Common:~%~s~]~
                          ~@[~&Extra:~%~s~]~
                          ~@[~&Missing:~%~s~]"
                           ',actual-body
                           (intersection ,actual ,expected :test #'equal)
                           (set-difference ,actual ,expected :test #'equal)
                           (set-difference ,expected ,actual :test #'equal)))
                     (unless relaxed-set-p
                       `((is-true (length= ,actual (remove-duplicates ,actual :test #'equal))
                                  "not a set (has duplicates):~%~{~s~%~}-->~%~s"
                                  ',actual-body ,actual)))))
                   (t
                    `((is (equal ,expected ,actual)
                          "log mismatch:~%~{~s~%~}-->~%~a"
                          ',actual-body
                          (diff ,expected ,actual))))))))))

(defmacro expecting (&body body)
  "Main event checking macro to be used with LOGGED-FIXTURE.

  TBD: describe each output form here...

  The usage is:

(expecting
  (<< something)
  (<< more)
  (<<< another)
  (when whatever
    (<<< :a-keyword))
  ==>
  some-evaluated-expr
  (:result2 'value-of-more something-evaluated)
  ('another := 'value-of-another)
  :a-keyword

  ===

  (<<< even-more)
  ==>
  ('even-more := 'value-of-even-more))
"
  (let ((state :actual)
	(act '())
	(exp '())
	(result '()))
    (flet ((flush ()
	     (when act
	       (push (expand-expecting-1 (nreverse act) (nreverse exp))
		     result)
	       (setf act '() exp '()))))
      (dolist (item body)
	(case state
	  (:actual
	   (if (eq item '==>)
	       (setf state :expected)
	       (push item act)))
	  (:expected
	   (cond ((eq item '===)
		  (flush)
		  (setf state :actual))
		 (t
		  (push item exp))))))
      (unless (or (eq :expected state)
		  (null act))
	(error "malformed body of EXPECTING"))
      (flush)
      (if (null (rest result))
	  (first result)
	  `(progn ,@(nreverse result))))))

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
