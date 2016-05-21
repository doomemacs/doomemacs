;;; defuns-go.el

;; TODO Implement doom:go-get-package
(defun doom--go-get-package ())

;;;###autoload
(defun doom:go-test-run-all ()
  (interactive)
  (async-shell-command (format "cd '%s' && go test" (doom/project-root))))

;;;###autoload
(defun doom:go-test-run-package ()
  (interactive)
  (error "Not yet implemented")
  (async-shell-command (format "cd '%s' && go test %s" (doom/project-root) (doom--go-get-package))))

(provide 'defuns-go)
;;; defuns-go.el ends here
