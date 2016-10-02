;;; defuns-elisp.el

;;;###autoload (autoload 'doom:byte-compile "defuns-elisp" nil t)
(evil-define-command doom:byte-compile (&optional bang)
  (interactive "<!>")
  (if bang (doom-byte-compile) (byte-compile-file buffer-file-name)))

;;;###autoload
(defun doom/elisp-find-function-at-pt ()
  (interactive)
  (let ((func (function-called-at-point)))
    (if func (find-function func))))

;;;###autoload
(defun doom/elisp-find-function-at-pt-other-window ()
  (interactive)
  (let ((func (function-called-at-point)))
    (if func (find-function-other-window func))))

(defsubst doom--ert-pre ()
  (save-buffer)
  (eval-buffer))

;;;###autoload
(defun doom/ert-run-test ()
  (interactive)
  (let (case-fold-search thing)
    (doom--ert-pre)
    (setq thing (thing-at-point 'defun t))
    (if thing
        (if (string-match "(ert-deftest \\([^ ]+\\)" thing)
            (ert-run-tests-interactively
             (substring thing (match-beginning 1) (match-end 1)))
          (user-error "Invalid test at point"))
      (user-error "No test found at point"))))

;;;###autoload
(defun doom/ert-rerun-test ()
  (interactive)
  (let (case-fold-search thing)
    (doom--ert-pre)
    (setq thing (car-safe ert--selector-history))
    (if thing
        (ert-run-tests-interactively thing)
      (message "No test found in history, looking for test at point")
      (doom/ert-run-test))))

;;;###autoload
(defun doom/ert-run-all-tests ()
  (interactive)
  (ert-delete-all-tests)
  (doom--ert-pre)
  (ert-run-tests-interactively t))

;;;###autoload
(defun doom/elisp-inf-ielm ()
  (ielm)
  (let ((buf (current-buffer)))
    (bury-buffer)
    (pop-to-buffer buf)))

(provide 'defuns-elisp)
;;; defuns-elisp.el ends here
