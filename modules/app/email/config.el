;;; app/email/config.el -*- lexical-binding: t; -*-

;; I want to live in Emacs. Living is incomplete without email, so Emacs needs
;; to give me the ability to read, search, write and send my email. It does so
;; with `mu4e', and requires `offlineimap' and `mu' to be installed.

(defvar +email-mu4e-mail-path "~/.mail"
  "The directory path of mu's maildir.")


;;
;; Config
;;

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
       (when-let (address (cdr (assq 'user-mail-address account-vars)))
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
                         ("\\\\Draft" "Drafts" ?d)
                         ("flag:unread AND \\\\Inbox" "Unread messages" ?u)
                         ("flag:flagged" "Starred messages" ?s)
                         ("date:today..now" "Today's messages" ?t)
                         ("date:7d..now" "Last 7 days" ?w)
                         ("mime:image/*" "Messages with images" ?p)))

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

  ;; In my workflow, emails won't be moved at all. Only their flags/labels are
  ;; changed. Se we redefine the trash and refile marks not to do any moving.
  ;; However, the real magic happens in `+email|gmail-fix-flags'.
  ;;
  ;; Gmail will handle the rest.
  (setq mu4e-marks (assq-delete-all 'delete mu4e-marks))
  (setq mu4e-marks (assq-delete-all 'trash mu4e-marks))
  (push '(trash :char ("d" . "▼")
                :prompt "dtrash"
                :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
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
  ;; Without it, refiling (archiving), trashing, and flagging (starring) email
  ;; won't properly result in the corresponding gmail action, since the marks
  ;; are ineffectual otherwise.
  (defun +email|gmail-fix-flags (mark msg)
    (cond ((eq mark 'trash) (mu4e-action-retag-message msg "-\\Inbox,+\\Trash,-\\Draft"))
          ((eq mark 'refile) (mu4e-action-retag-message msg "-\\Inbox"))
          ((eq mark 'flag) (mu4e-action-retag-message msg "+\\Starred"))
          ((eq mark 'unflag) (mu4e-action-retag-message msg "-\\Starred"))))
  (add-hook 'mu4e-mark-execute-pre-hook #'+email|gmail-fix-flags)

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
    (cl-loop for str in '((mu4e-main-mode . normal)
                          (mu4e-view-mode . normal)
                          (mu4e-headers-mode . normal)
                          (mu4e-compose-mode . normal)
                          (mu4e~update-mail-mode . normal))
             do (evil-set-initial-state (car str) (cdr str)))

    (setq mu4e-view-mode-map (make-sparse-keymap)
          ;; mu4e-compose-mode-map (make-sparse-keymap)
          mu4e-headers-mode-map (make-sparse-keymap)
          mu4e-main-mode-map (make-sparse-keymap))

    (map! (:map (mu4e-main-mode-map mu4e-view-mode-map)
            :leader
            :n "," #'mu4e-context-switch
            :n "." #'mu4e-headers-search-bookmark
            :n ">" #'mu4e-headers-search-bookmark-edit
            :n "/" #'mu4e~headers-jump-to-maildir)

          (:map (mu4e-headers-mode-map mu4e-view-mode-map)
            :localleader
            :n "f" #'mu4e-compose-forward
            :n "r" #'mu4e-compose-reply
            :n "c" #'mu4e-compose-new
            :n "e" #'mu4e-compose-edit)

          (:map mu4e-main-mode-map
            :n "q"   #'mu4e-quit
            :n "u"   #'mu4e-update-index
            :n "U"   #'mu4e-update-mail-and-index
            :n "J"   #'mu4e~headers-jump-to-maildir
            :n "c"   #'+email/compose
            :n "b"   #'mu4e-headers-search-bookmark)

          (:map mu4e-headers-mode-map
            :n "q"   #'mu4e~headers-quit-buffer
            :n "r"   #'mu4e-compose-reply
            :n "c"   #'mu4e-compose-edit
            :n "s"   #'mu4e-headers-search-edit
            :n "S"   #'mu4e-headers-search-narrow
            :n "RET" #'mu4e-headers-view-message
            :n "u"   #'mu4e-headers-mark-for-unmark
            :n "U"   #'mu4e-mark-unmark-all
            :n "v"   #'evil-visual-line
            :nv "d"  #'+email/mark
            :nv "="  #'+email/mark
            :nv "-"  #'+email/mark
            :nv "+"  #'+email/mark
            :nv "!"  #'+email/mark
            :nv "?"  #'+email/mark
            :nv "r"  #'+email/mark
            :nv "m"  #'+email/mark
            :n "x"   #'mu4e-mark-execute-all

            :n "]]"  #'mu4e-headers-next-unread
            :n "[["  #'mu4e-headers-prev-unread

            (:localleader
              :n "s" 'mu4e-headers-change-sorting
              :n "t" 'mu4e-headers-toggle-threading
              :n "r" 'mu4e-headers-toggle-include-related

              :n "%" #'mu4e-headers-mark-pattern
              :n "t" #'mu4e-headers-mark-subthread
              :n "T" #'mu4e-headers-mark-thread))

          (:map mu4e-view-mode-map
            :n "q" #'mu4e~view-quit-buffer
            :n "r" #'mu4e-compose-reply
            :n "c" #'mu4e-compose-edit

            :n "<M-Left>"  #'mu4e-view-headers-prev
            :n "<M-Right>" #'mu4e-view-headers-next
            :n "[m" #'mu4e-view-headers-prev
            :n "]m" #'mu4e-view-headers-next
            :n "[u" #'mu4e-view-headers-prev-unread
            :n "]u" #'mu4e-view-headers-next-unread

            (:localleader
              :n "%" #'mu4e-view-mark-pattern
              :n "t" #'mu4e-view-mark-subthread
              :n "T" #'mu4e-view-mark-thread

              :n "d" #'mu4e-view-mark-for-trash
              :n "r" #'mu4e-view-mark-for-refile
              :n "m" #'mu4e-view-mark-for-move))

          (:map mu4e~update-mail-mode-map
            :n "q" #'mu4e-interrupt-update-mail))))


(def-package! mu4e-maildirs-extension
  :after mu4e
  :config (mu4e-maildirs-extension-load))


(def-package! org-mu4e
  :commands org-mu4e-compose-org-mode
  :init (add-hook 'mu4e-compose-mode-hook #'org-mu4e-compose-org-mode)
  :config
  (setq org-mu4e-link-query-in-headers-mode nil
        org-mu4e-convert-to-html t)

  ;; Only render to html once. If the first send fails for whatever reason,
  ;; org-mu4e would do so each time you try again.
  (add-hook! 'message-send-hook
    (setq-local org-mu4e-convert-to-html nil)))

