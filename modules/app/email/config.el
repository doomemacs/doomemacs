;;; app/email/config.el -*- lexical-binding: t; -*-

;; I want to live in Emacs. Living is incomplete without email, so Emacs needs
;; to give me the ability to read, search, write and send my email. It does so
;; with `mu4e', and requires `offlineimap' and `mu' to be installed.

(defvar +email-backend 'mbsync
  "Which backend to use. Can either be offlineimap, mbsync or nil (manual).")

(def-setting! :email (label letvars &optional default-p)
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
  `(after! mu4e
     (let ((account-vars ,letvars))
       (when-let* ((address (cdr (assq 'user-mail-address account-vars))))
         (cl-pushnew address mu4e-user-mail-address-list :test #'equal))
       (let ((context (make-mu4e-context
                       :name ,label
                       :enter-func (lambda () (mu4e-message "Switched to %s" ,label))
                       :leave-func (lambda () (mu4e-clear-caches))
                       :match-func
                       (lambda (msg)
                         (when msg
                           (string-prefix-p (format "/%s" ,label)
                                            (mu4e-message-field msg :maildir))))
                       :vars ,letvars)))
         (push context mu4e-contexts)
         ,(when default-p
            `(setq-default mu4e-context-current context))))))


;;
;; Plugins
;;

(def-package! mu4e
  :commands (mu4e mu4e-compose-new)
  :init
  (setq mu4e-maildir "~/.mail"
        mu4e-attachment-dir "~/.mail/.attachments"
        mu4e-user-mail-address-list nil)
  :config
  (pcase +email-backend
    (`mbsync
     (setq mu4e-get-mail-command "mbsync -a"
           mu4e-change-filenames-when-moving t))
    (`offlineimap
     (setq mu4e-get-mail-command "offlineimap -o -q")))

  (setq mu4e-update-interval nil
        mu4e-compose-format-flowed t ; visual-line-mode + auto-fill upon sending
        mu4e-view-show-addresses t
        mu4e-sent-messages-behavior 'sent
        mu4e-index-cleanup nil
        mu4e-index-lazy-check t
        mu4e-hide-index-messages t
        ;; try to show images
        mu4e-view-show-images t
        mu4e-view-image-max-width 800
        ;; configuration for sending mail
        message-send-mail-function #'smtpmail-send-it
        smtpmail-stream-type 'starttls
        message-kill-buffer-on-exit t ; close after sending
        ;; start with the first (default) context;
        mu4e-context-policy 'pick-first
        ;; compose with the current context, or ask
        mu4e-compose-context-policy 'ask-if-none
        ;; use helm/ivy
        mu4e-completing-read-function
        (cond ((featurep! :completion ivy) #'ivy-completing-read)
              ((featurep! :completion helm) #'completing-read)
              (t #'ido-completing-read))
        ;; no need to ask
        mu4e-confirm-quit t
        ;; remove 'lists' column
        mu4e-headers-fields
        '((:account . 12)
          (:human-date . 12)
          (:flags . 4)
          (:from . 25)
          (:subject)))

  ;; Use fancy icons
  (setq mu4e-headers-has-child-prefix '("+" . "")
        mu4e-headers-empty-parent-prefix '("-" . "")
        mu4e-headers-first-child-prefix '("-" . "")
        mu4e-headers-duplicate-prefix '("-" . "")
        mu4e-headers-default-prefix '("-" . "")
        mu4e-headers-draft-mark '("-" . "")
        mu4e-headers-flagged-mark '("-" . "")
        mu4e-headers-new-mark '("-" . "")
        mu4e-headers-passed-mark '("-" . "")
        mu4e-headers-replied-mark '("-" . "")
        mu4e-headers-seen-mark '("-" . "")
        mu4e-headers-trashed-mark '("-" . "")
        mu4e-headers-attach-mark '("-" . "")
        mu4e-headers-encrypted-mark '("-" . "")
        mu4e-headers-signed-mark '("-" . "")
        mu4e-headers-unread-mark '("-" . ""))

  ;; Add a column to display what email account the email belongs to.
  (add-to-list 'mu4e-header-info-custom
               '(:account
                 :name "Account"
                 :shortname "Account"
                 :help "Which account this email belongs to"
                 :function
                 (lambda (msg)
                   (let ((maildir (mu4e-message-field msg :maildir)))
                     (format "%s" (substring maildir 1 (string-match-p "/" maildir 1)))))))

  ;; Refresh the current view after marks are executed
  (defun +email*refresh (&rest _) (mu4e-headers-rerun-search))
  (advice-add #'mu4e-mark-execute-all :after #'+email*refresh)

  (when (featurep! :feature spellcheck)
    (add-hook 'mu4e-compose-mode-hook #'flyspell-mode))

  ;; Wrap text in messages
  (add-hook! 'mu4e-view-mode-hook
    (setq-local truncate-lines nil))

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (after! evil
    (dolist (mode '(mu4e-main-mode mu4e-view-mode mu4e-headers-mode
                    mu4e-compose-mode mu4e~update-mail-mode))
      (evil-set-initial-state mode 'normal))))


(def-package! mu4e-maildirs-extension
  :after mu4e
  :config
  (mu4e-maildirs-extension-load)
  (setq mu4e-maildirs-extension-title nil
        ;; mu4e-maildirs-extension-ignored-regex "^*~*"
        mu4e-maildirs-extension-buffer-name "*Mail*"
        mu4e-maildirs-extension-insert-before-str "\n\t[c] Compose a message"
        mu4e-maildirs-extension-action-text "\t[g] Update mail and index\n"
        mu4e-maildirs-extension-maildir-expanded-prefix "-"
        mu4e-maildirs-extension-maildir-default-prefix "|"))


(def-package! org-mu4e
  :hook (mu4e-compose-mode . org-mu4e-compose-org-mode)
  :config
  (setq org-mu4e-link-query-in-headers-mode nil
        org-mu4e-convert-to-html t)

  ;; Only render to html once. If the first send fails for whatever reason,
  ;; org-mu4e would do so each time you try again.
  (add-hook! 'message-send-hook
    (setq-local org-mu4e-convert-to-html nil)))


;;
;; Sub-modules
;;

(if (featurep! +gmail) (load! +gmail))
