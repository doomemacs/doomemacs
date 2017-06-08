;;; app/email/autoload/email.el -*- lexical-binding: t; -*-

;;;###autoload
(defun =email ()
  "Start email client."
  (interactive)
  (call-interactively #'mu4e))

;;;###autoload
(defun +email/compose ()
  "Compose a new email."
  (interactive)
  ;; TODO Interactively select email account
  (call-interactively #'mu4e-compose-new))

