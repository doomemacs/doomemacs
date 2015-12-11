;;; defuns-spaceline.el

;;;###autoload
(defun narf|spaceline-env-update ()
  (when narf--env-command
    (let ((default-directory (narf/project-root)))
      (let ((s (shell-command-to-string narf--env-command)))
        (setq narf--env-version (if (string-match "[ \t\n\r]+\\'" s)
                                    (replace-match "" t t s)
                                  s))))))

(provide 'defuns-spaceline)
;;; defuns-spaceline.el ends here
