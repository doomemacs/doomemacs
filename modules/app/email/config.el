;;; app/email/config.el

;; I want to live in Emacs. Living is incomplete without email, so Emacs needs
;; to give me the ability to read, search, write and send my email. It does so
;; with `mu4e', and requires `offlineimap' and `mu' to be installed.

(defvar +email-mu4e-mail-path "~/.mail"
  "TODO")


;;
;;
;;

(defvar +email--accounts nil)

(def-setting! :email (label letvars &optional default)
  "TODO"
  (let* ((name (or (cdr (assq 'user-full-name letvars)) user-full-name))
         (address (cdr (assq 'user-mail-address letvars))))
    (dolist (var letvars letvars)
      (let ((val (cdr var)))
        (when (and (stringp val) (string-match-p "%s" val))
          (setcdr var (format val label)))))
    `(progn
       (push ',(cons label letvars) +email--accounts)
       ,(when address
          `(add-to-list 'mu4e-user-mail-address-list ,address))
       (let ((context (make-mu4e-context
                       :name ,label
                       :match-func
                       (lambda (msg)
                         (when msg
                           (string-prefix-p (format "/%s" ,label) (mu4e-message-field msg :maildir))))
                       :vars ',letvars)))
         (push context mu4e-contexts)
         ,(when default
            `(setq-default mu4e-context-current context))))))


;;
;; Plugins
;;

(def-package! mu4e
  :commands (mu4e mu4e-compose-new)
  :config
  (setq mu4e-maildir +email-mu4e-mail-path
        mu4e-user-mail-address-list nil
        mu4e-update-interval nil
        mu4e-compose-format-flowed t ; visual-line-mode + auto-fill upon sending
        mu4e-view-show-addresses t
        ;; try to show images
        mu4e-show-images t
        mu4e-view-show-images t
        mu4e-view-image-max-width 800
        ;; rename files when moving (required for mbsync)
        mu4e-change-filenames-when-moving t
        ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
        mu4e-sent-messages-behavior 'delete
        ;; allow for updating mail using 'U' in the main view:
        mu4e-get-mail-command "offlineimap -o"
        ;; configuration for sending mail
        message-send-mail-function 'smtpmail-send-it
        smtpmail-stream-type 'starttls
        ;; start with the first (default) context;
        mu4e-context-policy 'pick-first
        ;; compose with the current context, or ask
        mu4e-compose-context-policy 'ask-if-none
        ;; use helm/ivy
        mu4e-completing-read-function 'ido-completing-read
        ;; close message after sending it
        message-kill-buffer-on-exit t
        ;; no need to ask
        mu4e-confirm-quit nil
        ;; remove 'lists' column
        mu4e-headers-fields (assq-delete-all :mailing-list mu4e-headers-fields))

  (setq mu4e-marks (assq-delete-all 'trash mu4e-marks))
  (push '(trash :char ("d" . "▼")
                :prompt "dtrash"
                :dyn-target
                (lambda (target msg) (mu4e-get-trash-folder msg))
                :action
                (lambda (docid msg target)
                  (mu4e~proc-move docid (mu4e~mark-check-target target) "-N")))
        mu4e-marks)
  (push '(read :char ("!" . "◻")
               :prompt "!read"
               :show-target (lambda (target) "read")
               :action
               (lambda (docid msg target) (mu4e~proc-move docid nil "+S-N")))
        mu4e-marks)

  (after! doom-themes
    (add-hook 'mu4e-view-mode-hook 'doom-buffer-mode))

  (add-hook! 'mu4e-view-mode-hook
    (setq-local truncate-lines nil))

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types)))


(def-package! evil-mu4e
  :after mu4e)


(def-package! mu4e-alert
  :after mu4e
  :config
  (cond (IS-MAC   (mu4e-alert-set-default-style 'notifier))
        (IS-LINUX (mu4e-alert-set-default-style 'libnotify)))
  (mu4e-alert-enable-notifications))


(def-package! mu4e-maildirs-extension
  :after mu4e
  :config (mu4e-maildirs-extension-load))


(def-package! org-mu4e
  :commands org-mu4e-compose-org-mode
  :init (add-hook 'mu4e-compose-mode-hook 'org-mu4e-compose-org-mode)
  :config
  (setq org-mu4e-link-query-in-headers-mode nil
        org-mu4e-convert-to-html t)
  (add-hook! 'message-send-hook
    (setq-local org-mu4e-convert-to-html nil)))

