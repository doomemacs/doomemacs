;;; defuns-spaceline.el

;;;###autoload
(defun narf|spaceline-env-update ()
  (when narf--env-command
    (let* ((command (format "cd '%s' && %s" (narf/project-root) narf--env-command))
           (s (shell-command-to-string command)))
      (setq narf--env-version (if (string-match "[ \t\n\r]+\\'" s)
                                  (replace-match "" t t s)
                                s)))))

(provide 'defuns-spaceline)
;;; defuns-spaceline.el ends here
