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
        mu4e-headers-thread-single-orphan-prefix '("─>" . "─▶")
        mu4e-headers-thread-orphan-prefix '("┬>" . "┬▶ ")
        mu4e-headers-thread-last-child-prefix '("└>" . "╰▶")
        mu4e-headers-thread-child-prefix '("├>" . "├▶")
        mu4e-headers-thread-connection-prefix '("│" . "│ ")
        ;; remove 'lists' column
        mu4e-headers-fields
        '((:account . 12)
          (:human-date . 8)
          (:flags . 6) ; 3 icon flags
          (:from . 25)
          (:subject)))

  ;; set mail user agent
  (setq mail-user-agent 'mu4e-user-agent
        message-mail-user-agent 'mu4e-user-agent)

  ;; Set the icons only when a graphical frame has been created
  (if (display-graphic-p)
      (mu4e~initialise-icons)
    ;; When it's the server, wait till the first graphical frame
    (add-hook! 'server-after-make-frame-hook
      (defun mu4e~initialise-icons-hook ()
        (when (display-graphic-p)
          (mu4e~initialise-icons)
          (remove-hook #'mu4e~initialise-icons-hook)))))

  (plist-put (cdr (assoc :flags mu4e-header-info)) :shortname " Flags") ; default=Flgs
  (add-to-list 'mu4e-bookmarks
               '(:name "Flagged messages" :query "flag:flagged" :key ?f))

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
                          'face 'mu4e-footer-face))))))

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

  (map! :map mu4e-headers-mode-map
        :e "l" #'mu4e-msg-to-agenda)

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

  (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

  (advice-add #'mu4e~main-action-str :override #'mu4e~main-action-prettier-str))

(use-package! org-msg
  :after mu4e
  :when (featurep! :lang org)
  :config
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t tex:dvipng"
        org-msg-startup "hidestars indent inlineimages"
        org-msg-greeting-fmt "\nHi %s,\n\n"
        org-msg-greeting-name-limit 3
        org-msg-text-plain-alternative t)

  (defvar org-msg-currently-exporting nil
    "Helper variable to indicate whether org-msg is currently exporting the org buffer to HTML.
Usefull for affecting HTML export config.")
  (defadvice! org-msg--now-exporting (&rest _)
    :before #'org-msg-org-to-xml
    (setq org-msg-currently-exporting t))
  (defadvice! org-msg--not-exporting (&rest _)
    :after #'org-msg-org-to-xml
    (setq org-msg-currently-exporting nil))

  (advice-add #'org-html-latex-fragment    :override #'org-html-latex-fragment-scaled)
  (advice-add #'org-html-latex-environment :override #'org-html-latex-environment-scaled)

  (defvar mu4e-compose--org-msg-toggle-next t ; t to initialise org-msg
    "Whether to toggle ")
  (defun mu4e-compose-org-msg-handle-toggle (toggle-p)
    (when (xor toggle-p mu4e-compose--org-msg-toggle-next)
      (org-msg-mode (if org-msg-mode -1 1))
      (setq mu4e-compose--org-msg-toggle-next
            (not mu4e-compose--org-msg-toggle-next))))

  (defadvice! mu4e-maybe-toggle-org-msg (orig-fn toggle-p)
    :around #'mu4e-compose-new
    :around #'mu4e-compose-reply
    (interactive "p")
    (mu4e-compose-org-msg-handle-toggle (/= 1 toggle-p))
    (funcall orig-fn))

  (defvar +org-msg-accent-color "#c01c28"
    "Accent color to use in org-msg's generated CSS.
Must be set before org-msg is loaded to take effect.")
  (setq org-msg-enforce-css
        (let* ((font-family '(font-family . "-apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Oxygen, Ubuntu, Cantarell,\
        \"Fira Sans\", \"Droid Sans\", \"Helvetica Neue\", Arial, sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\";"))
               (monospace-font '(font-family . "SFMono-Regular, Menlo, Monaco, Consolas, \"Liberation Mono\", \"Courier New\", monospace;"))
               (font-size '(font-size . "11pt"))
               (font `(,font-family ,font-size))
               (line-height '(line-height . "1.2"))
               (theme-color +org-msg-accent-color)
               (bold '(font-weight . "bold"))
               (color `(color . ,theme-color))
               (table `((margin-top . "6px") (margin-bottom . "6px")
                        (border-left . "none") (border-right . "none")
                        (border-top . "2px solid #222222")
                        (border-bottom . "2px solid #222222")
                        ))
               (ftl-number `(,color ,bold (text-align . "left")))
               (inline-modes '(asl c c++ conf cpp csv diff ditaa emacs-lisp
                                   fundamental ini json makefile man org plantuml
                                   python sh xml))
               (inline-src `((background-color . "rgba(27,31,35,.05)")
                             (border-radius . "3px")
                             (padding . ".2em .4em")
                             (font-size . "90%") ,monospace-font
                             (margin . 0)))
               (code-src
                (mapcar (lambda (mode)
                          `(code ,(intern (concat "src src-" (symbol-name mode)))
                                 ,inline-src))
                        inline-modes)))
          `((del nil ((color . "grey") (border-left . "none")
                      (text-decoration . "line-through") (margin-bottom . "0px")
                      (margin-top . "10px") (line-height . "11pt")))
            (a nil (,color))
            (a reply-header ((color . "black") (text-decoration . "none")))
            (div reply-header ((padding . "3.0pt 0in 0in 0in")
                               (border-top . "solid #e1e1e1 1.0pt")
                               (margin-bottom . "20px")))
            (span underline ((text-decoration . "underline")))
            (li nil (,line-height (margin-bottom . "0px")
                                  (margin-top . "2px")))
            (nil org-ul ((list-style-type . "square")))
            (nil org-ol (,@font ,line-height (margin-bottom . "0px")
                                (margin-top . "0px") (margin-left . "30px")
                                (padding-top . "0px") (padding-left . "5px")))
            (nil signature (,@font (margin-bottom . "20px")))
            (blockquote nil ((padding . "2px 12px") (margin-left . "10px")
                             (margin-top . "10px") (margin-bottom . "0")
                             (border-left . "3px solid #ccc")
                             (font-style . "italic")
                             (background . "#f9f9f9")))
            (p blockquote  ((margin . "0") (padding . "4px 0")))
            (code nil (,font-size ,monospace-font (background . "#f9f9f9")))
            ,@code-src
            (nil linenr ((padding-right . "1em")
                         (color . "black")
                         (background-color . "#aaaaaa")))
            (pre nil ((line-height . "1.2")
                      (color . ,(doom-color 'fg))
                      (background-color . ,(doom-color 'bg))
                      (margin . "4px 0px 8px 0px")
                      (padding . "8px 12px")
                      (width . "95%")
                      (border-radius . "5px")
                      (font-weight . "500")
                      ,monospace-font))
            (div org-src-container ((margin-top . "10px")))
            (nil figure-number ,ftl-number)
            (nil table-number)
            (caption nil ((text-align . "left")
                          (background . ,theme-color)
                          (color . "white")
                          ,bold))
            (nil t-above ((caption-side . "top")))
            (nil t-bottom ((caption-side . "bottom")))
            (nil listing-number ,ftl-number)
            (nil figure ,ftl-number)
            (nil org-src-name ,ftl-number)
            (img nil ((vertical-align . "middle")
                      (max-width . "100%")))
            (img latex-fragment-inline ((margin . "0 0.1em")))
            (table nil (,@table ,line-height (border-collapse . "collapse")))
            (th nil ((border . "none") (border-bottom . "1px solid #222222")
                     (background-color . "#EDEDED") (font-weight . "500")
                     (padding . "3px 10px")))
            (td nil (,@table (padding . "1px 10px")
                             (background-color . "#f9f9f9") (border . "none")))
            (td org-left ((text-align . "left")))
            (td org-right ((text-align . "right")))
            (td org-center ((text-align . "center")))
            (kbd nil ((border . "1px solid #d1d5da") (border-radius . "3px")
                      (box-shadow . "inset 0 -1px 0 #d1d5da")
                      (background-color . "#fafbfc") (color . "#444d56")
                      (padding . "3px 5px") (display . "inline-block")))
            (div outline-text-4 ((margin-left . "15px")))
            (div outline-4 ((margin-left . "10px")))
            (h4 nil ((margin-bottom . "0px") (font-size . "11pt")))
            (h3 nil ((margin-bottom . "0px")
                     ,color (font-size . "14pt")))
            (h2 nil ((margin-top . "20px") (margin-bottom . "20px")
                     ,color (font-size . "18pt")))
            (h1 nil ((margin-top . "20px") (margin-bottom . "0px")
                     ,color (font-size . "24pt")))
            (p nil ((text-decoration . "none") (line-height . "11pt")
                    (margin-top . "10px") (margin-bottom . "0px")
                    ,font-size (max-width . "100ch")))
            (b nil ((font-weight . "500") (color . ,theme-color)))
            (div nil (,@font (line-height . "12pt")))))))


;;
;;; Gmail integration

(when (featurep! +gmail)
  (after! mu4e
    ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
    (setq mu4e-sent-messages-behavior
          (lambda ()
            (if (string-match-p "@gmail.com\\'" (message-sendmail-envelope-from))
                'delete 'sent))

          ;; don't need to run cleanup after indexing for gmail
          mu4e-index-cleanup nil

          ;; because gmail uses labels as folders we can use lazy check since
          ;; messages don't really "move"
          mu4e-index-lazy-check t)

    (defun +mu4e-msg-gmail-p (msg)
      (or
       (string-match-p "gmail"
                       (cond
                        ((member (mu4e-message-field msg :to)
                                 (plist-get mu4e~server-props :personal-addresses))
                         (mu4e-message-field msg :to))
                        ((member (mu4e-message-field msg :from)
                                 (plist-get mu4e~server-props :personal-addresses))
                         (mu4e-message-field msg :from))
                        (t "")))
       (string-match-p "gmail" (mu4e-message-field msg :maildir))))

    ;; In my workflow, emails won't be moved at all. Only their flags/labels are
    ;; changed. Se we redefine the trash and refile marks not to do any moving.
    ;; However, the real magic happens in `+mu4e|gmail-fix-flags'.
    ;;
    ;; Gmail will handle the rest.
    (defun +mu4e--mark-seen (docid _msg target)
      (mu4e~proc-move docid (mu4e~mark-check-target target) "+S-u-N"))

    (defvar +mu4e--last-invalid-gmail-action 0)

    (delq! 'delete mu4e-marks #'assq)
    (setf (alist-get 'delete mu4e-marks)
          (list
           :char '("D" . "✘")
           :prompt "Delete"
           :show-target (lambda (target) "delete")
           :action (lambda (docid msg target)
                     (if (+mu4e-msg-gmail-p msg)
                         (progn (message "The delete operation is invalid for Gmail accounts. Trashing instead.")
                                (+mu4e--mark-seen docid msg target)
                                (when (< 2 (- (float-time) +mu4e--last-invalid-gmail-action))
                                  (sit-for 1))
                                (setq +mu4e--last-invalid-gmail-action (float-time)))
                       (mu4e~proc-remove docid))))
          (alist-get 'trash mu4e-marks)
          (list :char '("d" . "▼")
                :prompt "dtrash"
                :dyn-target (lambda (_target msg) (mu4e-get-trash-folder msg))
                :action (lambda (docid msg target)
                          (if (+mu4e-msg-gmail-p msg)
                              (+mu4e--mark-seen docid msg target)
                            (mu4e~proc-move docid (mu4e~mark-check-target target) "+T-N"))))
          ;; Refile will be my "archive" function.
          (alist-get 'refile mu4e-marks)
          (list :char '("r" . "▼")
                :prompt "rrefile"
                :dyn-target (lambda (_target msg) (mu4e-get-refile-folder msg))
                :action (lambda (docid msg target)
                          (if (+mu4e-msg-gmail-p msg)
                              (+mu4e--mark-seen docid msg target)
                            (mu4e~proc-move docid (mu4e~mark-check-target target) "-N")))
                #'+mu4e--mark-seen))

    ;; This hook correctly modifies gmail flags on emails when they are marked.
    ;; Without it, refiling (archiving), trashing, and flagging (starring) email
    ;; won't properly result in the corresponding gmail action, since the marks
    ;; are ineffectual otherwise.
    (add-hook! 'mu4e-mark-execute-pre-hook
      (defun +mu4e-gmail-fix-flags-h (mark msg)
        (when (+mu4e-msg-gmail-p msg)
          (pcase mark
            (`trash  (mu4e-action-retag-message msg "-\\Inbox,+\\Trash,-\\Draft"))
            (`delete (mu4e-action-retag-message msg "-\\Inbox,+\\Trash,-\\Draft"))
            (`refile (mu4e-action-retag-message msg "-\\Inbox"))
            (`flag   (mu4e-action-retag-message msg "+\\Starred"))
            (`unflag (mu4e-action-retag-message msg "-\\Starred"))))))))
