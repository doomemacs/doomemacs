;;; private/hlissner/config.el -*- lexical-binding: t; -*-

(when (featurep! :feature evil)
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


;;
(after! doom-themes
  ;; Since Fira Mono doesn't have an italicized variant, highlight it instead
  (set-face-attribute 'italic nil
                      :weight 'ultra-light
                      :foreground "#ffffff"
                      :background (doom-color 'current-line)))


(after! evil-mc
  ;; if I'm in insert mode, chances are I want cursors to resume
  (add-hook! 'evil-mc-before-cursors-created
    (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors nil t))
  (add-hook! 'evil-mc-after-cursors-deleted
    (remove-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors t)))


;; Don't use default snippets, use mine.
(after! yasnippet
  (setq yas-snippet-dirs
        (append (list '+hlissner-snippets-dir)
                (delq 'yas-installed-snippets-dir yas-snippet-dirs))))


;; app/irc
(setq +irc-notifications-watch-strings '("v0" "vnought" "hlissner"))

(set! :irc "irc.snoonet.org"
  `(:tls t
    :nick "v0"
    :port 6697
    :sasl-username ,(+pass-get-user "irc/snoonet.org")
    :sasl-password ,(+pass-get-secret "irc/snoonet.org")
    :channels (:after-auth "#ynought")))


;; app/email
(setq smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(set! :email "gmail.com"
  '((mu4e-sent-folder       . "/gmail.com/Sent Mail")
    (mu4e-drafts-folder     . "/gmail.com/Drafts")
    (mu4e-trash-folder      . "/gmail.com/Trash")
    (mu4e-refile-folder     . "/gmail.com/All Mail")
    (smtpmail-smtp-user     . "hlissner")
    (user-mail-address      . "hlissner@gmail.com")
    (mu4e-compose-signature . "---\nHenrik")))

(set! :email "lissner.net"
  '((mu4e-sent-folder       . "/lissner.net/Sent Mail")
    (mu4e-drafts-folder     . "/lissner.net/Drafts")
    (mu4e-trash-folder      . "/lissner.net/Trash")
    (mu4e-refile-folder     . "/lissner.net/All Mail")
    (smtpmail-smtp-user     . "henrik@lissner.net")
    (user-mail-address      . "henrik@lissner.net")
    (mu4e-compose-signature . "---\nHenrik Lissner"))
  t)
