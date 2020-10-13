;;; email/mu4e/autoload/email.el -*- lexical-binding: t; -*-

;;;###autodef
(defun set-email-account! (label letvars &optional default-p)
  "Registers an email address for mu4e. The LABEL is a string. LETVARS are a
list of cons cells (VARIABLE . VALUE) -- you may want to modify:

 + `user-full-name' (this or the global `user-full-name' is required)
 + `user-mail-address' (required in mu4e < 1.4)
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
    (when (version< mu4e-mu-version "1.4")
      (when-let (address (cdr (assq 'user-mail-address letvars)))
        (add-to-list 'mu4e-user-mail-address-list address)))
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



(defvar +mu4e-workspace-name "*mu4e*"
  "TODO")
(defvar +mu4e--old-wconf nil)

(add-hook 'mu4e-main-mode-hook #'+mu4e-init-h)

;;;###autoload
(defun =mu4e ()
  "Start email client."
  (interactive)
  (require 'mu4e)
  (if (featurep! :ui workspaces)
      ;; delete current workspace if empty
      ;; this is useful when mu4e is in the daemon
      ;; as otherwise you can accumulate empty workspaces
      (progn
        (unless (+workspace-buffer-list)
          (+workspace-delete (+workspace-current-name)))
        (+workspace-switch +mu4e-workspace-name t))
    (setq +mu4e--old-wconf (current-window-configuration))
    (delete-other-windows)
    (switch-to-buffer (doom-fallback-buffer)))
  (mu4e~start 'mu4e~main-view)
  ;; (save-selected-window
  ;;   (prolusion-mail-show))
  )

;;;###autoload
(defun +mu4e/compose ()
  "Compose a new email."
  (interactive)
  ;; TODO Interactively select email account
  (call-interactively #'mu4e-compose-new))

;; Icons need a bit of work
;; Spacing needs to be determined and adjucted
;;;###autoload
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

;;;###autoload
(cl-defun +mu4e-normalised-icon (name &key set colour height v-adjust)
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
;;;###autoload
(defun +mu4e-initialise-icons ()
  (setq mu4e-use-fancy-chars t
        mu4e-headers-draft-mark      (cons "D" (+mu4e-normalised-icon "pencil"))
        mu4e-headers-flagged-mark    (cons "F" (+mu4e-normalised-icon "flag"))
        mu4e-headers-new-mark        (cons "N" (+mu4e-normalised-icon "sync" :set "material" :height 0.8 :v-adjust -0.10))
        mu4e-headers-passed-mark     (cons "P" (+mu4e-normalised-icon "arrow-right"))
        mu4e-headers-replied-mark    (cons "R" (+mu4e-normalised-icon "arrow-right"))
        mu4e-headers-seen-mark       (cons "S" "") ;(+mu4e-normalised-icon "eye" :height 0.6 :v-adjust 0.07 :colour "dsilver"))
        mu4e-headers-trashed-mark    (cons "T" (+mu4e-normalised-icon "trash"))
        mu4e-headers-attach-mark     (cons "a" (+mu4e-normalised-icon "file-text-o" :colour "silver"))
        mu4e-headers-encrypted-mark  (cons "x" (+mu4e-normalised-icon "lock"))
        mu4e-headers-signed-mark     (cons "s" (+mu4e-normalised-icon "certificate" :height 0.7 :colour "dpurple"))
        mu4e-headers-unread-mark     (cons "u" (+mu4e-normalised-icon "eye-slash" :v-adjust 0.05))))

;;;###autoload
(defun +mu4e-header-colourise (str)
  (let* ((str-sum (apply #'+ (mapcar (lambda (c) (% c 3)) str)))
         (colour (nth (% str-sum (length mu4e-header-colourised-faces))
                      mu4e-header-colourised-faces)))
    (put-text-property 0 (length str) 'face colour str)
    str))

;; Adding emails to the agenda
;; Perfect for when you see an email you want to reply to
;; later, but don't want to forget about
;;;###autoload
(defun +mu4e-msg-to-agenda (arg)
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

;;;###autoload
(defun +mu4e-set-account ()
  "Set the account for composing a message. If a 'To' header is present,
and correspands to an email account, this account will be selected.
Otherwise, the user is prompted for the account they wish to use."
  (unless (and mu4e-compose-parent-message
               (let ((to (cdr (car (mu4e-message-field mu4e-compose-parent-message :to))))
                     (from (cdr (car (mu4e-message-field mu4e-compose-parent-message :from)))))
                 (if (member to (plist-get mu4e~server-props :personal-addresses))
                     (setq user-mail-address to)
                   (if (member from (plist-get mu4e~server-props :personal-addresses))
                       (setq user-mail-address from)
                     nil))))
    (ivy-read "Account: " (plist-get mu4e~server-props :personal-addresses) :action (lambda (candidate) (setq user-mail-address candidate)))))

;;;###autoload
(defun +mu4e~main-action-str-prettier (str &optional func-or-shortcut)
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
    newstr))

;;
;; Hooks

(defun +mu4e-init-h ()
  (add-hook 'kill-buffer-hook #'+mu4e-kill-mu4e-h nil t))

(defun +mu4e-kill-mu4e-h ()
  ;; (prolusion-mail-hide)
  (cond
   ((and (featurep! :ui workspaces) (+workspace-exists-p +mu4e-workspace-name))
    (+workspace/delete +mu4e-workspace-name))

   (+mu4e--old-wconf
    (set-window-configuration +mu4e--old-wconf)
    (setq +mu4e--old-wconf nil))))

;; org-msg hooks

;;;###autoload
(defun +org-msg-img-scale-css (img-uri)
  "For a given IMG-URI, use imagemagik to find its width."
  (if org-msg-currently-exporting
      (list :width
            (format "%.1fpx"
                    (/ (string-to-number
                        (shell-command-to-string
                         ;; TODO change to use 'file'
                         (format "identify -format %%w %s"
                                 (substring img-uri 7)))) ; 7=(length "file://")
                       (plist-get org-format-latex-options :scale))))
    (list :style (format "transform: scale(%.3f)"
                         (/ 1.0 (plist-get org-format-latex-options :scale))))))

;;;###autoload
(defun +org-html-latex-fragment-scaled (latex-fragment _contents info)
  "Transcode a LATEX-FRAGMENT object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information.

This differs from `org-html-latex-fragment' in that it uses the LaTeX fragment
as a meaningful alt value, applies a class to indicate what sort of fragment it is
(latex-fragment-inline or latex-fragment-block), and (on Linux) scales the image to
account for the value of :scale in `org-format-latex-options'."
  (let ((latex-frag (org-element-property :value latex-fragment))
        (processing-type (plist-get info :with-latex)))
    (cond
     ((memq processing-type '(t mathjax))
      (org-html-format-latex latex-frag 'mathjax info))
     ((memq processing-type '(t html))
      (org-html-format-latex latex-frag 'html info))
     ((assq processing-type org-preview-latex-process-alist)
      (let ((formula-link
             (org-html-format-latex latex-frag processing-type info)))
        (when (and formula-link (string-match "file:\\([^]]*\\)" formula-link))
          (let ((source (org-export-file-uri (match-string 1 formula-link)))
                (attributes (list :alt latex-frag
                                  :class (concat "latex-fragment-"
                                                 (if (equal "\\(" (substring latex-frag 0 2))
                                                     "inline" "block")))))
            (when (and (memq processing-type '(dvipng convert))
                       (not IS-WINDOWS) ; relies on posix path
                       (executable-find "identify"))
              (apply #'plist-put attributes (+org-msg-img-scale-css source)))
            (org-html--format-image source attributes info)))))
     (t latex-frag))))

;;;###autoload
(defun +org-html-latex-environment-scaled (latex-environment _contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information.

This differs from `org-html-latex-environment' in that (on Linux) it
scales the image to account for the value of :scale in `org-format-latex-options'."
  (let ((processing-type (plist-get info :with-latex))
        (latex-frag (org-remove-indentation
                     (org-element-property :value latex-environment)))
        (attributes (org-export-read-attribute :attr_html latex-environment))
        (label (and (org-element-property :name latex-environment)
                    (org-export-get-reference latex-environment info)))
        (caption (and (org-html--latex-environment-numbered-p latex-environment)
                      (number-to-string
                       (org-export-get-ordinal
                        latex-environment info nil
                        (lambda (l _)
                          (and (org-html--math-environment-p l)
                               (org-html--latex-environment-numbered-p l))))))))
    (plist-put attributes :class "latex-environment")
    (cond
     ((memq processing-type '(t mathjax))
      (org-html-format-latex
       (if (org-string-nw-p label)
           (replace-regexp-in-string "\\`.*"
                                     (format "\\&\n\\\\label{%s}" label)
                                     latex-frag)
         latex-frag)
       'mathjax info))
     ((assq processing-type org-preview-latex-process-alist)
      (let ((formula-link
             (org-html-format-latex
              (org-html--unlabel-latex-environment latex-frag)
              processing-type info)))
        (when (and formula-link (string-match "file:\\([^]]*\\)" formula-link))
          (let ((source (org-export-file-uri (match-string 1 formula-link))))
            (when (and (memq processing-type '(dvipng convert))
                       (not IS-WINDOWS) ; relies on posix path
                       (executable-find "identify"))
              (apply #'plist-put attributes (+org-msg-img-scale-css source)))
            (org-html--wrap-latex-environment
             (org-html--format-image source attributes info)
             info caption label)))))
     (t (org-html--wrap-latex-environment latex-frag info caption label)))))

;;
;; Cooperative locking

(defvar +mu4e-lock-file "/tmp/mu4e_lock"
  "Location of the lock file which stores the PID of the process currenty running mu4e")
(defvar +mu4e-lock-request-file "/tmp/mu4e_lock_request"
  "Location of the lock file for which creating indicated that another process wants the lock to be released")

(defvar +mu4e-lock-greedy nil
  "Whether to 'grab' the `+mu4e-lock-file' if nobody else has it, i.e. start Mu4e")
(defvar +mu4e-lock-relaxed nil
  "Whether if someone else wants the lock (signaled via `+mu4e-lock-request-file'), we should stop Mu4e and let go of it")

;;;###autoload
(defun +mu4e-lock-pid-info ()
  "Get info on the PID refered to in `+mu4e-lock-file' in the form (pid . process-attributes)
 If the file or process do not exist, the lock file is deleted an nil returned."
  (when (file-exists-p +mu4e-lock-file)
    (let* ((pid (string-to-number (f-read-text +mu4e-lock-file 'utf-8)))
           (process (process-attributes pid)))
      (if process (cons pid process)
        (delete-file +mu4e-lock-file) nil))))

;;;###autoload
(defun +mu4e-lock-avalible (&optional strict)
  "If the `+mu4e-lock-file' is avalible (unset or owned by this emacs) return t.
If STRICT only accept an unset lock file."
  (not (when-let* ((lock-info (+mu4e-lock-pid-info))
                   (pid (car lock-info)))
         (when (or strict (/= (emacs-pid) pid)) t))))

;;;###autoload
(defun +mu4e-lock-file-delete-maybe ()
  "Check `+mu4e-lock-file', and delete it if this process is responsible for it."
  (when (+mu4e-lock-avalible)
    (delete-file +mu4e-lock-file)
    (file-notify-rm-watch +mu4e-lock--request-watcher)))

;;;###autoload
(defun +mu4e-lock-start (orig-fun &optional callback)
  "Check `+mu4e-lock-file', and if another process is responsible for it, abort starting.
Else, write to this process' PID to the lock file"
  (unless (+mu4e-lock-avalible)
    (shell-command (format "touch %s" +mu4e-lock-request-file))
    (message "Lock file exists, requesting that it be given up")
    (sleep-for 0.1)
    (delete-file +mu4e-lock-request-file))
  (if (not (+mu4e-lock-avalible))
      (user-error "Unfortunately another Emacs is already doing stuff with Mu4e, and you can only have one at a time")
    (f-write-text (number-to-string (emacs-pid)) 'utf-8 +mu4e-lock-file)
    (delete-file +mu4e-lock-request-file)
    (funcall orig-fun callback)
    (setq +mu4e-lock--request-watcher
          (file-notify-add-watch +mu4e-lock-request-file
                                 '(change)
                                 #'+mu4e-lock-request))))

(defvar +mu4e-lock--file-watcher nil)
(defvar +mu4e-lock--file-just-deleted nil)
(defvar +mu4e-lock--request-watcher nil)

;;;###autoload
(defun +mu4e-lock-add-watcher ()
  (setq +mu4e-lock--file-just-deleted nil)
  (file-notify-rm-watch +mu4e-lock--file-watcher)
  (setq +mu4e-lock--file-watcher
        (file-notify-add-watch +mu4e-lock-file
                               '(change)
                               #'++mu4e-lock-file-updated)))

;;;###autoload
(defun +mu4e-lock-request (event)
  "Handle another process requesting the Mu4e lock."
  (when (equal (nth 1 event) 'created)
    (when +mu4e-lock-relaxed
      (mu4e~stop)
      (file-notify-rm-watch +mu4e-lock--file-watcher)
      (message "Someone else wants to use Mu4e, releasing lock")
      (delete-file +mu4e-lock-file)
      (run-at-time 0.2 nil #'+mu4e-lock-add-watcher))
    (delete-file +mu4e-lock-request-file)))

;;;###autoload
(defun ++mu4e-lock-file-updated (event)
  (if +mu4e-lock--file-just-deleted
      (+mu4e-lock-add-watcher)
    (when (equal (nth 1 event) 'deleted)
      (setq +mu4e-lock--file-just-deleted t)
      (when (and +mu4e-lock-greedy (+mu4e-lock-avalible t))
        (message "Noticed Mu4e lock was avalible, grabbed it")
        (run-at-time 0.2 nil #'mu4e~start)))))
