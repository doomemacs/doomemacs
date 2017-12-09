;;; private/hlissner/config.el -*- lexical-binding: t; -*-

(defvar +hlissner-dir (file-name-directory load-file-name))
(defvar +hlissner-snippets-dir (expand-file-name "snippets/" +hlissner-dir))

;;
(when (featurep! :feature evil)
  (load! +bindings)  ; my key bindings
  (load! +commands)) ; my custom ex commands

(setq epa-file-encrypt-to user-mail-address
      auth-sources (list (expand-file-name ".authinfo.gpg" +hlissner-dir))
      +doom-modeline-buffer-file-name-style 'relative-from-project)

(defun +hlissner*no-authinfo-for-tramp (orig-fn &rest args)
  "Don't look into .authinfo for local sudo TRAMP buffers."
  (let ((auth-sources (if (equal tramp-current-method "sudo") nil auth-sources)))
    (apply orig-fn args)))
(advice-add #'tramp-read-passwd :around #'+hlissner*no-authinfo-for-tramp)

(after! smartparens
  ;; Auto-close more conservatively
  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "(" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "[" nil :post-handlers '(("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p)))

;; feature/evil
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
(after! circe
  (setq +irc-notifications-watch-strings '("v0" "vnought" "hlissner"))

  (set! :irc "irc.snoonet.org"
    `(:tls t
      :nick "v0"
      :port 6697
      :sasl-username ,(+pass-get-user "irc/snoonet.org")
      :sasl-password ,(+pass-get-secret "irc/snoonet.org")
      :channels (:after-auth "#ynought"))))

;; app/email
(after! mu4e
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
    t))

;; The standard unicode characters are usually misaligned depending on the font.
;; This bugs me. Personally, markdown #-marks for headlines are more elegant, so
;; we use those.
(after! org-bullets
  (setq org-bullets-bullet-list '("#")))

;; Hide header lines in helm. I don't like them
(after! helm
  (set-face-attribute 'helm-source-header nil :height 0.1))
