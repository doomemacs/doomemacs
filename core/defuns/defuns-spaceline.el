;;; defuns-spaceline.el

;;;###autoload
(defun narf|spaceline-env-update ()
  (when narf--env-command
    (let ((default-directory (narf/project-root)))
      (let ((s (shell-command-to-string narf--env-command)))
        (setq narf--env-version (if (string-match "[ \t\n\r]+\\'" s)
                                    (replace-match "" t t s)
                                  s))))))

;;;###autoload
(defun narf/-flycheck-count (state)
  "Return flycheck information for the given error type STATE."
  (when (flycheck-has-current-errors-p state)
    (if (eq 'running flycheck-last-status-change)
        "?"
      (cdr-safe (assq state (flycheck-count-errors flycheck-current-errors))))))

(provide 'defuns-spaceline)
;;; defuns-spaceline.el ends here
