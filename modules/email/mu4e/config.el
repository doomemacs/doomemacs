;;; email/mu4e/config.el -*- lexical-binding: t; -*-

(defvar +mu4e-backend 'mbsync
  "Which backend to use. Can either be offlineimap, mbsync or nil (manual).")


;;
;;; Packages

(use-package! mu4e
  :commands mu4e mu4e-compose-new
  :init
  (provide 'html2text) ; disable obsolete package
  (when (or (not (require 'mu4e-meta nil t))
            (version< mu4e-mu-version "1.4"))
    (setq mu4e-maildir "~/.mail"
          mu4e-user-mail-address-list nil))
  (setq mu4e-attachment-dir
        (lambda (&rest _)
          (expand-file-name ".attachments" (mu4e-root-maildir))))
  :config
  (pcase +mu4e-backend
    (`mbsync
     (setq mu4e-get-mail-command "mbsync -a"
           mu4e-change-filenames-when-moving t))
    (`offlineimap
     (setq mu4e-get-mail-command "offlineimap -o -q")))

  (setq mu4e-update-interval nil
        mu4e-compose-format-flowed t ; visual-line-mode + auto-fill upon sending
        mu4e-view-show-addresses t
        mu4e-sent-messages-behavior 'sent
        mu4e-hide-index-messages t
        ;; try to show images
        mu4e-view-show-images t
        mu4e-view-image-max-width 800
        mu4e-view-use-gnus t ; the way of the future: https://github.com/djcb/mu/pull/1442#issuecomment-591695814
        ;; configuration for sending mail
        message-send-mail-function #'smtpmail-send-it
        smtpmail-stream-type 'starttls
        message-kill-buffer-on-exit t ; close after sending
        ;; start with the first (default) context;
        mu4e-context-policy 'pick-first
        ;; compose with the current context, or ask
        mu4e-compose-context-policy 'ask-if-none
        ;; use helm/ivy/vertico
        mu4e-completing-read-function
        (cond ((featurep! :completion ivy)     #'ivy-completing-read)
              ((featurep! :completion helm)    #'completing-read)
              ((featurep! :completion vertico) #'completing-read)
              (t #'ido-completing-read))
        ;; no need to ask
        mu4e-confirm-quit nil
        ;; remove 'lists' column
        mu4e-headers-fields
        '((:account . 12)
          (:human-date . 12)
          (:flags . 4)
          (:from . 25)
          (:subject)))

  ;; set mail user agent
  (setq mail-user-agent 'mu4e-user-agent)

  ;;----------------
  ;; Icons
  ;;----------------

  ;; Icons need a bit of work
  ;; Spacing needs to be determined and adjucted

  (defun +get-string-width (str)
    "Return the width in pixels of a string in the current
window's default font. If the font is mono-spaced, this
will also be the width of all other printable characters."
    (let ((window (selected-window))
          (remapping face-remapping-alist))
      (with-temp-buffer
        (make-local-variable 'face-remapping-alist)
        (setq face-remapping-alist remapping)
        (set-window-buffer window (current-buffer))
        (insert str)
        (car (window-text-pixel-size)))))

  (cl-defun mu4e~normalised-icon (name &key set colour height v-adjust)
    "Convert :icon declaration to icon"
    (let* ((icon-set (intern (concat "all-the-icons-" (or set "faicon"))))
           (v-adjust (or v-adjust 0.02))
           (height (or height 0.8))
           (icon (if colour
                     (apply icon-set `(,name :face ,(intern (concat "all-the-icons-" colour)) :height ,height :v-adjust ,v-adjust))
                   (apply icon-set `(,name  :height ,height :v-adjust ,v-adjust))))
           (icon-width (+get-string-width icon))
           (space-width (+get-string-width " "))
           (space-factor (- 2 (/ (float icon-width) space-width))))
      (concat (propertize " " 'display `(space . (:width ,space-factor))) icon)))

  ;; Set up all the fancy icons
  (defun mu4e~initialise-icons ()
    (setq mu4e-use-fancy-chars t
          mu4e-headers-draft-mark      (cons "D" (mu4e~normalised-icon "pencil"))
          mu4e-headers-flagged-mark    (cons "F" (mu4e~normalised-icon "flag"))
          mu4e-headers-new-mark        (cons "N" (mu4e~normalised-icon "sync" :set "material" :height 0.8 :v-adjust -0.10))
          mu4e-headers-passed-mark     (cons "P" (mu4e~normalised-icon "arrow-right"))
          mu4e-headers-replied-mark    (cons "R" (mu4e~normalised-icon "arrow-right"))
          mu4e-headers-seen-mark       (cons "S" "") ;(mu4e~normalised-icon "eye" :height 0.6 :v-adjust 0.07 :colour "dsilver"))
          mu4e-headers-trashed-mark    (cons "T" (mu4e~normalised-icon "trash"))
          mu4e-headers-attach-mark     (cons "a" (mu4e~normalised-icon "file-text-o" :colour "silver"))
          mu4e-headers-encrypted-mark  (cons "x" (mu4e~normalised-icon "lock"))
          mu4e-headers-signed-mark     (cons "s" (mu4e~normalised-icon "certificate" :height 0.7 :colour "dpurple"))
          mu4e-headers-unread-mark     (cons "u" (mu4e~normalised-icon "eye-slash" :v-adjust 0.05))))

  ;; Set the icons only when a graphical frame has been created
  (if (display-graphic-p)
      (mu4e~initialise-icons)
    ;; When it's the server, wait till the first graphical frame
    (add-hook! 'server-after-make-frame-hook
      (defun mu4e~initialise-icons-hook ()
        (when (display-graphic-p)
          (mu4e~initialise-icons)
          (remove-hook #'mu4e~initialise-icons-hook)))))

  ;;----------------
  ;; Header view
  ;;----------------

  (setq mu4e-headers-fields
        '((:account . 12)
          (:human-date . 8)
          (:flags . 6) ; 3 icon flags
          (:from . 25)
          (:subject))

        (plist-put (cdr (assoc :flags mu4e-header-info)) :shortname " Flags") ; default=Flgs

  ;; A header colourisation function/variable could be handy
  (defun mu4e~header-colourise (str)
    (let* ((str-sum (apply #'+ (mapcar (lambda (c) (% c 3)) str)))
           (colour (nth (% str-sum (length mu4e-header-colourised-faces))
                        mu4e-header-colourised-faces)))
      (put-text-property 0 (length str) 'face colour str)
      str))

  (defvar mu4e~header-colourised-faces
    '(all-the-icons-lblue
      all-the-icons-purple
      all-the-icons-blue-alt
      all-the-icons-green
      all-the-icons-maroon
      all-the-icons-yellow
      all-the-icons-orange))

  ;; Add a column to display what email account the email belongs to.
  (setq mu4e-header-info-custom
        '((:account .
           (:name "account"
            :shortname "account"
            :help "which account this email belongs to"
            :function
            (lambda (msg)
              (let ((maildir
                     (mu4e-message-field msg :maildir)))
                (mu4e-header-colourise
                 (replace-regexp-in-string
                  "^gmail"
                  (propertize "g" 'face 'bold-italic)
                  (format "%s"
                          (substring maildir 1
                                     (string-match-p "/" maildir 1)))))))))
          (:recipnum .
           (:name "Number of recipients"
            :shortname " ⭷"
            :help "Number of recipients for this message"
            :function
            (lambda (msg)
              (propertize (format "%2d"
                                  (+ (length (mu4e-message-field msg :to))
                                     (length (mu4e-message-field msg :cc))))
                          'face 'mu4e-footer-face)))))))

  ;; Marks usually affect the current view
  (defadvice! +mu4e--refresh-current-view-a (&rest _)
    :after #'mu4e-mark-execute-all (mu4e-headers-rerun-search))

  ;; Wrap text in messages
  (setq-hook! 'mu4e-view-mode-hook truncate-lines nil)

  ;; Html mails might be better rendered in a browser
  (add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser))

  ;; The header view needs a certain amount of horizontal space to
  ;; actually show you all the information you want to see
  ;; so if the header view is entered from a narrow frame,
  ;; it's probably worth trying to expand it
  (defvar mu4e-min-header-frame-width 120
    "Minimum reasonable with for the header view.")
  (defun mu4e-widen-frame-maybe ()
    "Expand the frame with if it's less than `mu4e-min-header-frame-width'."
    (when (< (frame-width) mu4e-min-header-frame-width)
      (set-frame-width (selected-frame) mu4e-min-header-frame-width)))
  (add-hook 'mu4e-headers-mode-hook #'mu4e-widen-frame-maybe)

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; Adding emails to the agenda
  ;; Perfect for when you see an email you want to reply to
  ;; later, but don't want to forget about

  (defun mu4e-msg-to-agenda (arg)
  "Refile a message and add a entry in the agenda file with a
deadline.  Default deadline is today.  With one prefix, deadline
is tomorrow.  With two prefixes, select the deadline."
  (interactive "p")
  (let ((file (car org-agenda-files))
        (sec  "^* Email")
        (msg  (mu4e-message-at-point)))
    (when msg
      ;; put the message in the agenda
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          ;; find header section
          (goto-char (point-min))
          (when (re-search-forward sec nil t)
            (let (org-M-RET-may-split-line
                  (lev (org-outline-level))
                  (folded-p (invisible-p (point-at-eol))))
              ;; place the subheader
              (when folded-p (show-branches))    ; unfold if necessary
              (org-end-of-meta-data) ; skip property drawer
              (org-insert-todo-heading 1)        ; insert a todo heading
              (when (= (org-outline-level) lev)  ; demote if necessary
                (org-do-demote))
              ;; insert message and add deadline
              (insert (concat " Respond to "
                              "[[mu4e:msgid:"
                              (plist-get msg :message-id) "]["
                              (truncate-string-to-width
                               (caar (plist-get msg :from)) 25 nil nil t)
                              " - "
                              (truncate-string-to-width
                               (plist-get msg :subject) 40 nil nil t)
                              "]] "))
              (org-deadline nil
                            (cond ((= arg 1) (format-time-string "%Y-%m-%d"))
                                  ((= arg 4) "+1d")))

              (org-update-parent-todo-statistics)

              ;; refold as necessary
              (if folded-p
                  (progn
                    (org-up-heading-safe)
                    (hide-subtree))
                (hide-entry))))))
      ;; refile the message and update
      ;; (cond ((eq major-mode 'mu4e-view-mode)
      ;;        (mu4e-view-mark-for-refile))
      ;;       ((eq major-mode 'mu4e-headers-mode)
      ;;        (mu4e-headers-mark-for-refile)))
      (message "Refiled \"%s\" and added to the agenda for %s"
               (truncate-string-to-width
                (plist-get msg :subject) 40 nil nil t)
               (cond ((= arg 1) "today")
                     ((= arg 4) "tomorrow")
                     (t         "later"))))))

  (map! :map mu4e-headers-mode-map
        :e "l" #'mu4e-msg-to-agenda)

  ;; General keybindings

  (map! :localleader
        :map mu4e-compose-mode-map
        :desc "send and exit" "s" #'message-send-and-exit
        :desc "kill buffer"   "d" #'message-kill-buffer
        :desc "save draft"    "S" #'message-dont-send
        :desc "attach"        "a" #'mail-add-attachment)
  ;; Due to evil, none of the marking commands work when making a visual selection in
  ;; the headers view of mu4e. Without overriding any evil commands we may actually
  ;; want to use in and evil selection, this can be easily fixed.
  (when (featurep! :editor evil)
    (map! :map mu4e-headers-mode-map
          :v "*" #'mu4e-headers-mark-for-something
          :v "!" #'mu4e-headers-mark-for-read
          :v "?" #'mu4e-headers-mark-for-unread
          :v "u" #'mu4e-headers-mark-for-unmark
          :vn "l" #'mu4e-msg-to-agenda))

  ;;----------------
  ;; Sending mail
  ;;----------------

  (defun my-mu4e-set-account ()
    "Set the account for composing a message. If a 'To' header is present,
and correspands to an email account, this account will be selected.
Otherwise, the user is prompted for the accound they wish to use."
    (unless (and mu4e-compose-parent-message
                 (let ((to (cdr (car (mu4e-message-field mu4e-compose-parent-message :to))))
                       (from (cdr (car (mu4e-message-field mu4e-compose-parent-message :from)))))
                   (if (member to (plist-get mu4e~server-props :personal-addresses))
                       (setq user-mail-address to)
                     (if (member from (plist-get mu4e~server-props :personal-addresses))
                         (setq user-mail-address from)
                       nil))))
      (ivy-read "Account: " (plist-get mu4e~server-props :personal-addresses) :action (lambda (candidate) (setq user-mail-address candidate)))))

  (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

  ;;----------------
  ;; Prettifying mu4e:main
  ;;----------------

  (defadvice! mu4e~main-action-prettier-str (str &optional func-or-shortcut)
    "Highlight the first occurrence of [.] in STR.
If FUNC-OR-SHORTCUT is non-nil and if it is a function, call it
when STR is clicked (using RET or mouse-2); if FUNC-OR-SHORTCUT is
a string, execute the corresponding keyboard action when it is
clicked."
  :override #'mu4e~main-action-str
  (let ((newstr
         (replace-regexp-in-string
          "\\[\\(..?\\)\\]"
          (lambda(m)
            (format "%s"
                    (propertize (match-string 1 m) 'face '(mode-line-emphasis bold))))
          (replace-regexp-in-string "\t\\*" "\t⚫" str)))
        (map (make-sparse-keymap))
        (func (if (functionp func-or-shortcut)
                  func-or-shortcut
                (if (stringp func-or-shortcut)
                    (lambda()(interactive)
                      (execute-kbd-macro func-or-shortcut))))))
    (define-key map [mouse-2] func)
    (define-key map (kbd "RET") func)
    (put-text-property 0 (length newstr) 'keymap map newstr)
    (put-text-property (string-match "[A-Za-z].+$" newstr)
                       (- (length newstr) 1) 'mouse-face 'highlight newstr)
    newstr)))

(when (featurep! :lang org)
  (use-package! org-msg
    :hook (org-load . org-msg-mode)
    :config
    (setq org-msg-startup "inlineimages"
          org-msg-greeting-name-limit 3
          org-msg-text-plain-alternative t)))



;;
;;; Gmail integration

(when (featurep! +gmail)
  (after! mu4e
    ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
    (setq mu4e-sent-messages-behavior 'delete

          ;; don't need to run cleanup after indexing for gmail
          mu4e-index-cleanup nil

          ;; because gmail uses labels as folders we can use lazy check since
          ;; messages don't really "move"
          mu4e-index-lazy-check t)

    ;; In my workflow, emails won't be moved at all. Only their flags/labels are
    ;; changed. Se we redefine the trash and refile marks not to do any moving.
    ;; However, the real magic happens in `+mu4e|gmail-fix-flags'.
    ;;
    ;; Gmail will handle the rest.
    (defun +mu4e--mark-seen (docid _msg target)
      (mu4e~proc-move docid (mu4e~mark-check-target target) "+S-u-N"))

    (delq! 'delete mu4e-marks #'assq)
    (setf (alist-get 'trash mu4e-marks)
          (list :char '("d" . "▼")
                :prompt "dtrash"
                :dyn-target (lambda (_target msg) (mu4e-get-trash-folder msg))
                :action #'+mu4e--mark-seen)
          ;; Refile will be my "archive" function.
          (alist-get 'refile mu4e-marks)
          (list :char '("r" . "▼")
                :prompt "rrefile"
                :dyn-target (lambda (_target msg) (mu4e-get-refile-folder msg))
                :action #'+mu4e--mark-seen))

    ;; This hook correctly modifies gmail flags on emails when they are marked.
    ;; Without it, refiling (archiving), trashing, and flagging (starring) email
    ;; won't properly result in the corresponding gmail action, since the marks
    ;; are ineffectual otherwise.
    (add-hook! 'mu4e-mark-execute-pre-hook
      (defun +mu4e-gmail-fix-flags-h (mark msg)
        (pcase mark
          (`trash  (mu4e-action-retag-message msg "-\\Inbox,+\\Trash,-\\Draft"))
          (`refile (mu4e-action-retag-message msg "-\\Inbox"))
          (`flag   (mu4e-action-retag-message msg "+\\Starred"))
          (`unflag (mu4e-action-retag-message msg "-\\Starred")))))))
