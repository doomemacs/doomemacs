;;; core-os-linux.el --- Debian-specific settings

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(defun doom-open-with (&optional app-name path)
  (interactive)
  (error "Not yet implemented"))

(defun def-docset! (&rest _)
  (message "No docset function defined!"))

(provide 'core-os-linux)
;;; core-os-linux.el ends here
