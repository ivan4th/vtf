(in-package :vtf)

;;; CONVENIENCE

(defun run-tests (&optional (name t))
  (abt-reset)
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
