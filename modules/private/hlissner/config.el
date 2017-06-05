;;; private/hlissner/config.el

(when (featurep 'evil)
  (load! +bindings)  ; my key bindings
  (load! +commands)) ; my custom ex commands

(defvar +hlissner-dir
  (file-name-directory load-file-name))

(defvar +hlissner-snippets-dir
  (expand-file-name "snippets/" +hlissner-dir))

(setq epa-file-encrypt-to user-mail-address
      auth-sources (list (expand-file-name ".authinfo.gpg" +hlissner-dir)))

(defun +hlissner*no-authinfo-for-tramp (orig-fn &rest args)
  "Don't look into .authinfo for local sudo TRAMP buffers."
  (let ((auth-sources (if (equal tramp-current-method "sudo") nil auth-sources)))
    (apply orig-fn args)))
(advice-add #'tramp-read-passwd :around #'+hlissner*no-authinfo-for-tramp)


(after! evil-mc
  ;; if I'm in insert mode, chances are I want cursors to resume
  (add-hook! 'evil-mc-before-cursors-created
    (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors nil t))
  (add-hook! 'evil-mc-after-cursors-deleted
    (remove-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors t)))


;; Don't use default snippets, use mine.
(after! yasnippet
  (setq yas-snippet-dirs (append (list '+hlissner-snippets-dir)
                                 (delete 'yas-installed-snippets-dir yas-snippet-dirs))))


(after! mu4e
  (setq-default
   smtpmail-stream-type 'starttls
   smtpmail-default-smtp-server "smtp.gmail.com"
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587)

  (set! :email "gmail.com"
    '((mu4e-sent-folder       . "/%s/Sent Mail")
      (mu4e-drafts-folder     . "/%s/Drafts")
      (mu4e-trash-folder      . "/%s/Trash")
      (mu4e-refile-folder     . "/%s/All Mail")
      (smtpmail-smtp-user     . "hlissner")
      (user-mail-address      . "hlissner@gmail.com")
      (mu4e-compose-signature . "---\nHenrik")))

  (set! :email "lissner.net"
    '((mu4e-sent-folder       . "/%s/Sent Mail")
      (mu4e-drafts-folder     . "/%s/Drafts")
      (mu4e-trash-folder      . "/%s/Trash")
      (mu4e-refile-folder     . "/%s/All Mail")
      (smtpmail-smtp-user     . "henrik@lissner.net")
      (user-mail-address      . "henrik@lissner.net")
      (mu4e-compose-signature . "---\nHenrik Lissner"))
    t))
