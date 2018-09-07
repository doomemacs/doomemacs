;;; app/email/autoload/email.el -*- lexical-binding: t; -*-

;;;###autodef
(defun set-email-account! (label letvars &optional default-p)
  "Registers an email address for mu4e. The LABEL is a string. LETVARS are a
list of cons cells (VARIABLE . VALUE) -- you may want to modify:

 + `user-full-name' (this or the global `user-full-name' is required)
 + `user-mail-address' (required)
 + `smtpmail-smtp-user' (required for sending mail from Emacs)

OPTIONAL:
 + `mu4e-sent-folder'
 + `mu4e-drafts-folder'
 + `mu4e-trash-folder'
 + `mu4e-refile-folder'
 + `mu4e-compose-signature'

DEFAULT-P is a boolean. If non-nil, it marks that email account as the
default/fallback account."
  (after! mu4e
    (when-let* ((address (cdr (assq 'user-mail-address letvars))))
      (add-to-list 'mu4e-user-mail-address-list address))
    (setq mu4e-contexts
          (cl-loop for context in mu4e-contexts
                   unless (string= (mu4e-context-name context) label)
                   collect context))
    (let ((context (make-mu4e-context
                    :name label
                    :enter-func (lambda () (mu4e-message "Switched to %s" label))
                    :leave-func #'mu4e-clear-caches
                    :match-func
                    (lambda (msg)
                      (when msg
                        (string-prefix-p (format "/%s" label)
                                         (mu4e-message-field msg :maildir))))
                    :vars letvars)))
      (push context mu4e-contexts)
      (when default-p
        (setq-default mu4e-context-current context))
      context)))

;; FIXME obsolete :email
;;;###autoload
(def-setting! :email (label letvars &optional default-p)
  :obsolete set-email-account!
  `(set-email-acount! ,label ,letvars ,default-p))



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

(defun +email|init ()
  (add-hook 'kill-buffer-hook #'+email|kill-mu4e nil t))

(defun +email|kill-mu4e ()
  ;; (prolusion-mail-hide)
  (when (+workspace-exists-p +email-workspace-name)
    (+workspace/delete +email-workspace-name)))

