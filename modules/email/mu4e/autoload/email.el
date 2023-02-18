;;; email/mu4e/autoload/email.el -*- lexical-binding: t; -*-

;;;###autodef
(defun set-email-account! (label letvars &optional default-p)
  "Registers an email address for mu4e. The LABEL is a string. LETVARS are a
list of cons cells (VARIABLE . VALUE) -- you may want to modify:

 + `user-full-name' (used to populate the FROM field when composing mail)
 + `user-mail-address' (required in mu4e < 1.4)
 + `smtpmail-smtp-user' (required for sending mail from Emacs)

OPTIONAL:
 + `mu4e-sent-folder'
 + `mu4e-drafts-folder'
 + `mu4e-trash-folder'
 + `mu4e-refile-folder'
 + `mu4e-compose-signature'
 + `+mu4e-personal-addresses'

DEFAULT-P is a boolean. If non-nil, it marks that email account as the
default/fallback account."
  (after! mu4e
    (when (version< mu4e-mu-version "1.4")
      (when-let (address (cdr (assq 'user-mail-address letvars)))
        (add-to-list 'mu4e-user-mail-address-list address)))
    ;; remove existing context with same label
    (setq mu4e-contexts
          (cl-loop for context in mu4e-contexts
                   unless (string= (mu4e-context-name context) label)
                   collect context))
    (let ((context (make-mu4e-context
                    :name label
                    :enter-func
                    (lambda () (mu4e-message "Switched to %s" label))
                    :leave-func
                    (lambda () (progn (setq +mu4e-personal-addresses nil)
                                      (mu4e-clear-caches)))
                    :match-func
                    (lambda (msg)
                      (when msg
                        (string-prefix-p (format "/%s" label)
                                         (mu4e-message-field msg :maildir) t)))
                    :vars letvars)))
      (add-to-list 'mu4e-contexts context (not default-p))
      context)))


(defvar +mu4e-workspace-name "*mu4e*"
  "Name of the workspace created by `=mu4e', dedicated to mu4e.")
(defvar +mu4e--old-wconf nil)

(add-hook 'mu4e-main-mode-hook #'+mu4e-init-h)

;;;###autoload
(defun =mu4e ()
  "Start email client."
  (interactive)
  (require 'mu4e)
  (if (modulep! :ui workspaces)
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
  ;; At this point, we're going to guess what the user wants to do.
  ;; To help with this, we'll collect potentially-useful buffers.
  ;; Then we'll try to jump to in order of preference:
  ;; 1. A draft message buffer
  ;; 2. A viewed message buffer
  ;; 3. A headers buffer
  ;; If none of those exist and are appropiate to jump to,
  ;; `mu4e' will be called.
  (let (compose-buffer view-buffer headers-buffer)
    (dolist (buf (buffer-list))
      (pcase (buffer-local-value 'major-mode buf)
        ((and (or 'org-msg-edit-mode 'mu4e-compose-mode)
              (guard (not compose-buffer)))
         (setq compose-buffer buf))
        ((and 'mu4e-view-mode (guard (not view-buffer)))
         (setq view-buffer buf))
        ((and 'mu4e-headers-mode (guard (not headers-buffer)))
         (setq headers-buffer buf))))
    (if (and compose-buffer (not (eq compose-buffer (current-buffer))))
        (switch-to-buffer compose-buffer)
      (if (and view-buffer (not (eq view-buffer (current-buffer))))
          (switch-to-buffer view-buffer)
        (if (and headers-buffer (not (eq headers-buffer (current-buffer))))
            (switch-to-buffer headers-buffer)
          (mu4e))))))

;;;###autoload
(defun +mu4e/compose ()
  "Compose a new email."
  (interactive)
  ;; TODO Interactively select email account
  (call-interactively #'mu4e-compose-new))

(defun +mu4e--get-string-width (str)
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

(cl-defun +mu4e-normalised-icon (name &key set color height v-adjust)
  "Convert :icon declaration to icon"
  (let* ((icon-set (intern (concat "all-the-icons-" (or set "faicon"))))
         (v-adjust (or v-adjust 0.02))
         (height (or height 0.8))
         (icon (if color
                   (apply icon-set `(,name :face ,(intern (concat "all-the-icons-" color)) :height ,height :v-adjust ,v-adjust))
                 (apply icon-set `(,name  :height ,height :v-adjust ,v-adjust))))
         (icon-width (+mu4e--get-string-width icon))
         (space-width (+mu4e--get-string-width " "))
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
        mu4e-headers-replied-mark    (cons "R" (+mu4e-normalised-icon "reply"))
        mu4e-headers-seen-mark       (cons "S" "") ;(+mu4e-normalised-icon "eye" :height 0.6 :v-adjust 0.07 :color "dsilver"))
        mu4e-headers-trashed-mark    (cons "T" (+mu4e-normalised-icon "trash"))
        mu4e-headers-attach-mark     (cons "a" (+mu4e-normalised-icon "file-text-o" :color "silver"))
        mu4e-headers-encrypted-mark  (cons "x" (+mu4e-normalised-icon "lock"))
        mu4e-headers-signed-mark     (cons "s" (+mu4e-normalised-icon "certificate" :height 0.7 :color "dpurple"))
        mu4e-headers-unread-mark     (cons "u" (+mu4e-normalised-icon "eye-slash" :v-adjust 0.05))
        mu4e-headers-list-mark       (cons "l" (+mu4e-normalised-icon "sitemap" :set "faicon"))
        mu4e-headers-personal-mark   (cons "p" (+mu4e-normalised-icon "user"))
        mu4e-headers-calendar-mark   (cons "c" (+mu4e-normalised-icon "calendar"))))

(defun +mu4e-colorize-str (str &optional unique herring)
  "Apply a face from `+mu4e-header-colorized-faces' to STR.
If HERRING is set, it will be used to determine the face instead of STR.
Will try to make unique when non-nil UNIQUE,
a quoted symbol for a alist of current strings and faces provided."
  (unless herring
    (setq herring str))
  (put-text-property
   0 (length str)
   'face
   (list
    (if (not unique)
        (+mu4e--str-color-face herring str)
      (let ((unique-alist (eval unique)))
        (unless (assoc herring unique-alist)
          (if (> (length unique-alist) (length +mu4e-header-colorized-faces))
              (push (cons herring (+mu4e--str-color-face herring)) unique-alist)
            (let ((offset 0) color color?)
              (while (not color)
                (setq color? (+mu4e--str-color-face herring offset))
                (if (not (rassoc color? unique-alist))
                    (setq color color?)
                  (setq offset (1+ offset))
                  (when (> offset (length +mu4e-header-colorized-faces))
                    (message "Warning: +mu4e-colorize-str was called with non-unique-alist UNIQUE-alist alist.")
                    (setq color (+mu4e--str-color-face herring)))))
              (push (cons herring color) unique-alist)))
          (set unique unique-alist))
        (cdr (assoc herring unique-alist))))
    'default)
   str)
  str)

(defun +mu4e--str-color-face (str &optional offset)
  "Select a face from `+mu4e-header-colorized-faces' based on
STR and any integer OFFSET."
  (let* ((str-sum (apply #'+ (mapcar (lambda (c) (% c 3)) str)))
         (color (nth (% (+ str-sum (if offset offset 0))
                        (length +mu4e-header-colorized-faces))
                     +mu4e-header-colorized-faces)))
    color))

(defvar +org-capture-emails-file "todo.org"
  "Default target for storing mu4e emails captured from within mu4e.
Requires a \"* Email\" heading be present in the file.")

;; Adding emails to the agenda
;; Perfect for when you see an email you want to reply to
;; later, but don't want to forget about
;;;###autoload
(defun +mu4e/capture-msg-to-agenda (arg)
  "Refile a message and add a entry in `+org-capture-emails-file' with a
deadline.  Default deadline is today.  With one prefix, deadline
is tomorrow.  With two prefixes, select the deadline."
  (interactive "p")
  (let ((sec "^* Email")
        (msg (mu4e-message-at-point)))
    (when msg
      ;; put the message in the agenda
      (with-current-buffer (find-file-noselect
                            (expand-file-name +org-capture-emails-file org-directory))
        (save-excursion
          ;; find header section
          (goto-char (point-min))
          (when (re-search-forward sec nil t)
            (let (org-M-RET-may-split-line
                  (lev (org-outline-level))
                  (folded-p (invisible-p (point-at-eol)))
                  (from (plist-get msg :from)))
              (when (consp (car from)) ; Occurs when using mu4e 1.8+.
                (setq from (car from)))
              (unless (keywordp (car from)) ; If using mu4e <= 1.6.
                (setq from (list :name (or (caar from) (cdar from)))))
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
                               (plist-get from :name) 25 nil nil t)
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
(defun +mu4e/attach-files (&optional files-to-attach)
  "When called in a dired buffer, ask for a message to attach the marked files to.
When called in a mu4e:compose or org-msg buffer, `read-file-name'to either
attach a file, or select a folder to open dired in and select file attachments
(using `dired-mu4e-attach-ctrl-c-ctrl-c').

When otherwise called, open a dired buffer and enable `dired-mu4e-attach-ctrl-c-ctrl-c'."
  ;; TODO add ability to attach files (+dirs) as a single (named) archive
  (interactive "p")
  (+mu4e-compose-org-msg-handle-toggle (/= 1 files-to-attach))
  (pcase major-mode
    ((or 'mu4e-compose-mode 'org-msg-edit-mode)
     (let ((mail-buffer (current-buffer))
           (location (read-file-name "Attach: " nil nil t "")))
       (if (not (file-directory-p location))
           (pcase major-mode
             ('mu4e-compose-mode
              (save-excursion
                (goto-char (point-max))
                (unless (eq (current-column) 0)
                  (insert "\n\n")
                  (forward-line 2))
                (mail-add-attachment location)))
             ('org-msg-edit-mode (org-msg-attach-attach location)))
         (split-window-sensibly)
         (with-current-buffer (dired location)
           (setq-local dired-mail-buffer mail-buffer)
           (dired-mu4e-attach-ctrl-c-ctrl-c 1)))))
    ('dired-mode
     (unless (and files-to-attach (/= 1 files-to-attach))
       (setq files-to-attach
             (delq nil
                   (mapcar
                    ;; don't attach directories
                    (lambda (f) (if (file-directory-p f) nil f))
                    (nreverse (dired-map-over-marks (dired-get-filename) nil))))))
     (if (not files-to-attach)
         (progn
           (message "No files marked, aborting.")
           (kill-buffer-and-window))
       (if-let ((mail-target-buffer (bound-and-true-p dired-mail-buffer)))
           (progn (kill-buffer-and-window)
                  (switch-to-buffer mail-target-buffer))
         (if (and (+mu4e-current-buffers)
                  (y-or-n-p "Attach files to existing mail composition buffer? "))
             (progn (setf mail-target-buffer
                          (completing-read "Message: " (+mu4e-current-buffers)))
                    (kill-buffer-and-window)
                    (switch-to-buffer mail-target-buffer))
           (kill-buffer-and-window)
           (mu4e-compose 'new)))
       (mapcar
        (pcase major-mode
          ('mu4e-compose-mode #'mail-add-attachment)
          ('org-msg-edit-mode #'org-msg-attach-attach))
        files-to-attach)))
    (_
     (split-window-sensibly)
     (with-current-buffer (call-interactively #'find-file)
       (dired-mu4e-attach-ctrl-c-ctrl-c 1)))))

(define-minor-mode dired-mu4e-attach-ctrl-c-ctrl-c
  "Adds C-c C-c as a keybinding to attach files to a message."
  :lighter "attach"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") '+mu4e/attach-files)
            map)
  (setq header-line-format
        (when dired-mu4e-attach-ctrl-c-ctrl-c
          (substitute-command-keys
           "Mu4e attach active. `\\[+mu4e/attach-files]' to attach the marked files."))))

(defun +mu4e-current-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (or (and (derived-mode-p 'message-mode)
                       (null message-sent-message-via))
                  (eq major-mode 'org-msg-edit-mode))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

;;; Hooks

(defun +mu4e-init-h ()
  (add-hook 'kill-buffer-hook #'+mu4e-kill-mu4e-h nil t))

(defun +mu4e-kill-mu4e-h ()
  ;; (prolusion-mail-hide)
  (cond
   ((and (modulep! :ui workspaces) (+workspace-exists-p +mu4e-workspace-name))
    (+workspace/delete +mu4e-workspace-name))

   (+mu4e--old-wconf
    (set-window-configuration +mu4e--old-wconf)
    (setq +mu4e--old-wconf nil))))

;;;###autoload
(defun +mu4e-set-from-address-h ()
  "If the user defines multiple `+mu4e-personal-addresses' for email aliases
within a context, set `user-mail-address' to an alias found in the 'To' or
'From' headers of the parent message if present, or prompt the user for a
preferred alias"
  (when-let ((addresses (if (or mu4e-contexts +mu4e-personal-addresses)
                            (and (> (length +mu4e-personal-addresses) 1)
                                 +mu4e-personal-addresses)
                          (mu4e-personal-addresses))))
    (setq user-mail-address
          (if mu4e-compose-parent-message
              (if (version<= "1.8" mu4e-mu-version)
                  (let ((to (mu4e-message-field mu4e-compose-parent-message :to))
                        (cc (mu4e-message-field mu4e-compose-parent-message :cc))
                        (from (mu4e-message-field mu4e-compose-parent-message :from)))
                    (or (car (cl-intersection
                              (mapcar (lambda (adr) (plist-get adr :email))
                                      (append to from cc))
                              addresses
                              :test #'equal))
                        (completing-read "From: " addresses)))
                (let ((to (mapcar #'cdr (mu4e-message-field mu4e-compose-parent-message :to)))
                      (cc (mapcar #'cdr (mu4e-message-field mu4e-compose-parent-message :cc)))
                      (from (mapcar #'cdr (mu4e-message-field mu4e-compose-parent-message :from))))
                  (or (car (cl-intersection (append to from cc) addresses
                                            :test #'equal))
                      (completing-read "From: " addresses))))
            (completing-read "From: " addresses)))))
