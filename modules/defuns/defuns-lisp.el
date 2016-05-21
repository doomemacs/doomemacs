;;; defuns-lisp.el

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

(defun doom--ert-pre ()
  (save-buffer)
  (eval-buffer))

;;;###autoload
(defun doom/ert-run-test ()
  (interactive)
  (let (case-fold-search)
    (doom--ert-pre)
    (aif (thing-at-point 'defun t)
        (if (string-match "(ert-deftest \\([^ ]+\\)" it)
            (ert-run-tests-interactively (substring it (match-beginning 1) (match-end 1)))
          (user-error "Invalid test at point"))
      (user-error "No test found at point"))))

;;;###autoload
(defun doom/ert-rerun-test ()
  (interactive)
  (let (case-fold-search)
    (doom--ert-pre)
    (aif (car-safe ert--selector-history)
        (ert-run-tests-interactively it)
      (message "No test found in history, looking for test at point")
      (doom/ert-run-test))))

;;;###autoload
(defun doom/ert-run-all-tests ()
  (interactive)
  (ert-delete-all-tests)
  (doom--ert-pre)
  (ert-run-tests-interactively t))

;;;###autoload
(defun doom/elisp-auto-compile ()
  (when (let ((file-name (buffer-file-name)))
          (and (f-exists? (f-expand (concat (f-base file-name) ".elc") (f-dirname file-name)))
               (--any? (f-child-of? file-name it)
                       (append (list doom-core-dir doom-modules-dir
                                     doom-core-dir doom-modules-dir
                                     doom-private-dir)))))
    (doom:compile-el)))

;;;###autoload
(defun doom/elisp-inf-ielm ()
  (ielm)
  (let ((buf (current-buffer)))
    (bury-buffer)
    (pop-to-buffer buf)))

(provide 'defuns-lisp)
;;; defuns-lisp.el ends here
