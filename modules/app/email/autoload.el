;;; app/email/autoload.el

;;;###autoload
(defun =email ()
  (interactive)
  (call-interactively 'mu4e))

;;;###autoload
(defun +email/compose ()
  (interactive)
  (call-interactively 'mu4e-compose-new))

