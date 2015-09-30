;;; defuns-go.el

;; TODO Implement narf:go-get-package
(defun narf--go-get-package ())

;;;###autoload
(defun narf:go-test-run-all ()
  (interactive)
  (async-shell-command (format "cd '%s' && go test" (narf/project-root))))

;;;###autoload
(defun narf:go-test-run-package ()
  (interactive)
  (error "Not yet implemented")
  (async-shell-command (format "cd '%s' && go test %s" (narf/project-root) (narf--go-get-package))))

(provide 'defuns-go)
;;; defuns-go.el ends here
