;;; defuns-lisp.el

;;;###autoload
(defun narf/elisp-find-function-at-pt ()
  (interactive)
  (let ((func (function-called-at-point)))
    (if func (find-function func))))

;;;###autoload
(defun narf/elisp-find-function-at-pt-other-window ()
  (interactive)
  (let ((func (function-called-at-point)))
    (if func (find-function-other-window func))))

(defun narf--ert-pre ()
  (save-buffer)
  (eval-buffer))

;;;###autoload
(defun narf/ert-run-test ()
  (interactive)
  (let (case-fold-search)
    (narf--ert-pre)
    (aif (thing-at-point 'defun t)
        (if (string-match "(ert-deftest \\([^ ]+\\)" it)
            (ert-run-tests-interactively (substring it (match-beginning 1) (match-end 1)))
          (user-error "Invalid test at point"))
      (user-error "No test found at point"))))

;;;###autoload
(defun narf/ert-rerun-test ()
  (interactive)
  (let (case-fold-search)
    (narf--ert-pre)
    (aif (car-safe ert--selector-history)
        (ert-run-tests-interactively it)
      (message "No test found in history, looking for test at point")
      (narf/ert-run-test))))

;;;###autoload
(defun narf/ert-run-all-tests ()
  (interactive)
  (ert-delete-all-tests)
  (narf--ert-pre)
  (ert-run-tests-interactively t))

;;;###autoload
(defun narf/elisp-auto-compile ()
  (when (let ((file-name (buffer-file-name)))
          (and (f-exists? (f-expand (concat (f-base file-name) ".elc") (f-dirname file-name)))
               (--any? (f-child-of? file-name it)
                       (append (list narf-core-dir narf-modules-dir
                                     narf-core-dir narf-modules-dir
                                     narf-private-dir)))))
    (narf:compile-el)))

;;;###autoload
(defun narf/elisp-inf-ielm ()
  (ielm)
  (let ((buf (current-buffer)))
    (bury-buffer)
    (pop-to-buffer buf)))

(provide 'defuns-lisp)
;;; defuns-lisp.el ends here
