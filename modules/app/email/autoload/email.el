;;; app/email/autoload/email.el -*- lexical-binding: t; -*-

(defvar +email-workspace-name "*mu4e*"
  "TODO")

(add-hook 'mu4e-main-mode-hook #'+email|init)

;;;###autoload
(defun =email ()
  "Start email client."
  (interactive)
  (require 'mu4e)
  (+workspace-switch +email-workspace-name t)
  (mu4e~start 'mu4e~main-view)
  ;; (save-selected-window
  ;;   (prolusion-mail-show))
  )

;;;###autoload
(defun +email/compose ()
  "Compose a new email."
  (interactive)
  ;; TODO Interactively select email account
  (call-interactively #'mu4e-compose-new))


;;
;; Hooks
;;

(defun +email|init ()
  (add-hook 'kill-buffer-hook #'+email|kill-mu4e nil t))

(defun +email|kill-mu4e ()
  ;; (prolusion-mail-hide)
  (when (+workspace-exists-p +email-workspace-name)
    (+workspace/delete +email-workspace-name)))

