;;; app/email/config.el

;; I want to live in Emacs. Living is incomplete without email, so Emacs needs
;; to give me the ability to read, search, write and send my email. It does so
;; with `mu4e', and requires `offlineimap' and `mu' to be installed.

(defvar +email-mu4e-mail-path "~/.mail"
  "The directory path of mu's maildir.")


;;
;;
;;

(defvar +email--accounts nil)

(def-setting! :email (label letvars &optional default)
  "Registers an email address for mu4e."
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
                       :enter-func (lambda () (mu4e-message "Switched to %s" ,label))
                       :leave-func (lambda () (mu4e-clear-caches))
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
        mu4e-attachment-dir "~/Downloads"
        mu4e-user-mail-address-list nil
        mu4e-update-interval nil
        mu4e-compose-format-flowed t ; visual-line-mode + auto-fill upon sending
        mu4e-view-show-addresses t
        ;; try to show images
        mu4e-view-show-images t
        mu4e-view-image-max-width 800
        ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
        mu4e-sent-messages-behavior 'delete
        ;; allow for updating mail using 'U' in the main view:
        ;; for mbsync
        ;; mu4e-headers-skip-duplicates t
        ;; mu4e-change-filenames-when-moving nil
        ;; mu4e-get-mail-command "mbsync -a"
        ;; for offlineimap
        mu4e-get-mail-command "offlineimap -o -q"
        ;; configuration for sending mail
        message-send-mail-function #'smtpmail-send-it
        smtpmail-stream-type 'starttls
        ;; start with the first (default) context;
        mu4e-context-policy 'pick-first
        ;; compose with the current context, or ask
        mu4e-compose-context-policy 'ask-if-none
        ;; use helm/ivy
        mu4e-completing-read-function (cond ((featurep! :completion ivy) #'ivy-completing-read)
                                            ((featurep! :completion helm) #'completing-read)
                                            (t #'ido-completing-read))
        ;; close message after sending it
        message-kill-buffer-on-exit t
        ;; no need to ask
        mu4e-confirm-quit nil
        ;; remove 'lists' column
        mu4e-headers-fields
        '((:account . 12)
          (:human-date . 12)
          (:flags . 4)
          (:from . 25)
          (:subject))
        mu4e-bookmarks `(("\\\\Inbox" "Inbox" ?i)
                         ("flag:unread AND \\\\Inbox" "Unread messages" ?u)
                         ("flag:flagged" "Starred messages" ?s)
                         ("date:today..now" "Today's messages" ?t)
                         ("date:7d..now" "Last 7 days" ?w)
                         ("mime:image/*" "Messages with images" ?p)))

  ;; Add a column to display what email account the email belongs to.
  (push '(:account
          :name "Account"
          :shortname "Account"
          :help "Which account this email belongs to"
          :function
          (lambda (msg)
            (let ((maildir (mu4e-message-field msg :maildir)))
              (format "%s" (substring maildir 1 (string-match-p "/" maildir 1))))))
        mu4e-header-info-custom)

  ;; In my workflow, emails won't be moved at all. Only their flags/labels are
  ;; changed. Se we redefine the trash and refile marks not to do any moving.
  ;; However, the real magic happens in `+email|gmail-fix-flags'.
  ;;
  ;; Gmail will handle the rest.
  (setq mu4e-marks (assq-delete-all 'delete mu4e-marks))
  (setq mu4e-marks (assq-delete-all 'trash mu4e-marks))
  (push '(trash :char ("d" . "▼")
                :prompt "dtrash"
                :show-target (lambda (target) "trash")
                :action
                (lambda (docid msg target)
                  (mu4e~proc-move docid (mu4e~mark-check-target target) "+S-u-N")))
        mu4e-marks)

  ;; Refile will be my "archive" function.
  (setq mu4e-marks (assq-delete-all 'refile mu4e-marks))
  (push '(refile :char ("r" . "▶")
                 :prompt "refile"
                 :show-target (lambda (target) "archive")
                 :action
                 (lambda (docid msg target)
                   (mu4e~proc-move docid (mu4e~mark-check-target target) "+S-u-N")))
        mu4e-marks)

  ;; This hook correctly modifies gmail flags on emails when they are marked.
  ;; Without it refiling (archiving), trashing, and flagging (starring) email
  ;; won't properly result in the corresponding gmail action, since the marks
  ;; are ineffectual otherwise.
  (defun +email|gmail-fix-flags (mark msg)
    (cond ((eq mark 'trash) (mu4e-action-retag-message msg "-\\Inbox,+\\Trash"))
          ((memq mark '(read refile)) (mu4e-action-retag-message msg "-\\Inbox,+\\Seen"))
          ((eq mark 'flag) (mu4e-action-retag-message msg "+\\Starred"))
          ((eq mark 'unflag) (mu4e-action-retag-message msg "-\\Starred"))))
  (add-hook 'mu4e-mark-execute-pre-hook #'+email|gmail-fix-flags)

  ;; Refresh the current view after marks are executed
  (defun +email*refresh (&rest _) (mu4e-headers-rerun-search))
  (advice-add #'mu4e-mark-execute-all :after #'+email*refresh)

  (when (featurep! :feature spellcheck)
    (add-hook 'mu4e-compose-mode-hook #'flyspell-mode))

  ;; Brighter + no mode-line in message windows
  (after! doom-themes
    (add-hook 'mu4e-view-mode-hook #'doom-buffer-mode))

  ;; Wrap text in messages
  (add-hook! 'mu4e-view-mode-hook
    (setq-local truncate-lines nil))

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types)))


(def-package! evil-mu4e
  :after mu4e
  :when (featurep! :feature evil)
  :init (defvar evil-mu4e-state 'motion)
  :config
  (defun +email|headers-keybinds ()
    (map! :Lm "-"   #'mu4e-headers-mark-for-unflag
          :Lm "+"   #'mu4e-headers-mark-for-flag
          :Lm "v"   #'evil-visual-line
          :Lm "q"   #'mu4e~headers-quit-buffer
          ;; Enable multiple markings via visual mode
          :Lv "ESC" #'evil-motion-state
          :Lv "d"   #'+email/mark-multiple
          :Lv "-"   #'+email/mark-multiple
          :Lv "+"   #'+email/mark-multiple
          :Lv "!"   #'+email/mark-multiple
          :Lv "?"   #'+email/mark-multiple
          :Lv "r"   #'+email/mark-multiple))
  (add-hook 'mu4e-headers-mode-hook #'+email|headers-keybinds)

  (defun +email|view-keybinds ()
    (map! :Lm "-"   #'mu4e-headers-mark-for-unflag
          :Lm "+"   #'mu4e-headers-mark-for-flag
          :Lm "v"   #'evil-visual-line
          :Lm "q"   #'mu4e~headers-quit-buffer))
  (add-hook 'mu4e-view-mode-hook #'+email|view-keybinds)

  (defun +email|main-keybinds ()
    (map! :Lm "q" #'mu4e-quit
          :Lm "u" #'mu4e-update-index
          :Lm "U" #'mu4e-update-mail-and-index))
  (add-hook 'mu4e-main-mode-hook #'+email|main-keybinds))


(def-package! mu4e-maildirs-extension
  :after mu4e
  :config (mu4e-maildirs-extension-load))


(def-package! org-mu4e
  :commands org-mu4e-compose-org-mode
  :init (add-hook 'mu4e-compose-mode-hook #'org-mu4e-compose-org-mode)
  :config
  (setq org-mu4e-link-query-in-headers-mode nil
        org-mu4e-convert-to-html t)

  ;; Only render to html once. Without this, it would continue to do so, if the
  ;; first send failed for whatever reason.
  (add-hook! 'message-send-hook
    (setq-local org-mu4e-convert-to-html nil)))

