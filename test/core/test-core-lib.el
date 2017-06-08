;;; ../test/test-core-lib.el

(def-test-group! core-lib
  ;; `add-hook!'
  (ert-deftest add-one-to-one-hook ()
    (let (hooks)
      (add-hook! 'hooks 'a-hook)
      (should (equal hooks '(a-hook)))))

  (ert-deftest add-many-to-one-hook ()
    (let (hooks)
      (add-hook! 'hooks '(hook-a hook-b hook-c))
      (should (equal hooks '(hook-c hook-b hook-a)))))

  (ert-deftest add-one-to-many-hooks ()
    (let (hooks-a hooks-b hooks-c)
      (add-hook! '(hooks-a hooks-b hooks-c) 'a-hook)
      (should (equal hooks-a '(a-hook)))
      (should (equal hooks-b '(a-hook)))
      (should (equal hooks-c '(a-hook)))))

  (ert-deftest add-many-to-many-hooks ()
    (let (hooks-a hooks-b hooks-c)
      (add-hook! '(hooks-a hooks-b hooks-c) '(hook-a hook-b hook-c))
      (should (equal hooks-a '(hook-c hook-b hook-a)))
      (should (equal hooks-b '(hook-c hook-b hook-a)))
      (should (equal hooks-c '(hook-c hook-b hook-a)))))

  (ert-deftest add-non-literal-hooks ()
    (let (some-mode-hook)
      (add-hook! some-mode 'a-hook)
      (should (equal some-mode-hook '(a-hook)))))

  ;; `remove-hook!'
  (ert-deftest remove-hooks ()
    (let ((hooks-a '(hook-c hook-b hook-a))
          (hooks-b '(hook-c hook-b hook-a))
          (hooks-c '(hook-c hook-b hook-a)))
      (remove-hook! '(hooks-a hooks-b hooks-c) '(hook-a hook-b hook-c))
      (should (null hooks-a))
      (should (null hooks-b))
      (should (null hooks-c))))

  (ert-deftest remove-hook-forms ()
    (let (hooks)
      (add-hook! 'hooks (message "Hello world"))
      (should hooks)
      (remove-hook! 'hooks (message "Hello world"))
      (should (null hooks))))

  ;; `add-transient-hook!'
  (ert-deftest transient-hooks ()
    (let (hooks value)
      (add-transient-hook! 'hooks (setq value t))
      (run-hooks 'hooks)
      (should (eq value t))
      (should (null hooks))))

  (ert-deftest transient-function ()
    (let (value)
      (add-transient-hook! #'ignore (setq value (not value)))
      (ignore t)
      (should (eq value t))
      ;; repeat to ensure it was only run once
      (ignore t)
      (should (eq value t))))

  ;; TODO `associate!'

  ;; TODO `def-setting!' & `set!'

  )
