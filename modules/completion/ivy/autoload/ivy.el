;;; completion/ivy/autoload/ivy.el -*- lexical-binding: t; -*-

(defun +ivy--is-workspace-buffer-p (buffer)
  (let ((buffer (car buffer)))
    (when (stringp buffer)
      (setq buffer (get-buffer buffer)))
    (+workspace-contains-buffer-p buffer)))

(defun +ivy--is-workspace-other-buffer-p (buffer)
  (let ((buffer (car buffer)))
    (when (stringp buffer)
      (setq buffer (get-buffer buffer)))
    (and (not (eq buffer (current-buffer)))
         (+workspace-contains-buffer-p buffer))))

;;;###autoload
(defun +ivy-rich-buffer-name (candidate)
  "Display the buffer name.

Buffers that are considered unreal (see `doom-real-buffer-p') are dimmed with
`+ivy-buffer-unreal-face'."
  (let ((b (get-buffer candidate)))
    (when (null uniquify-buffer-name-style)
      (setq candidate (replace-regexp-in-string "<[0-9]+>$" "" candidate)))
    (cond ((ignore-errors
             (file-remote-p
              (buffer-local-value 'default-directory b)))
           (ivy-append-face candidate 'ivy-remote))
          ((doom-unreal-buffer-p b)
           (ivy-append-face candidate +ivy-buffer-unreal-face))
          ((not (buffer-file-name b))
           (ivy-append-face candidate 'ivy-subdir))
          ((buffer-modified-p b)
           (ivy-append-face candidate 'ivy-modified-buffer))
          (candidate))))

;;;###autoload
(defun +ivy-rich-buffer-icon (candidate)
  "Display the icon for CANDIDATE buffer."
  ;; NOTE This is inspired by `all-the-icons-ivy-buffer-transformer', minus the
  ;; buffer name and extra padding as those are handled by `ivy-rich'.
  (propertize "\t" 'display
              (if-let* ((buffer (get-buffer candidate))
                        (mode (buffer-local-value 'major-mode buffer)))
                  (or
                   (all-the-icons-ivy--icon-for-mode mode)
                   (all-the-icons-ivy--icon-for-mode (get mode 'derived-mode-parent))
                   (funcall
                    all-the-icons-ivy-family-fallback-for-buffer
                    all-the-icons-ivy-name-fallback-for-buffer))
                (all-the-icons-icon-for-file candidate))))

;;;###autoload
(defun +ivy-rich-describe-variable-transformer (cand)
  "Previews the value of the variable in the minibuffer"
  (let* ((sym (intern cand))
         (val (and (boundp sym) (symbol-value sym)))
         (print-level 3))
    (replace-regexp-in-string
     "[\n\t\^[\^M\^@\^G]" " "
     (cond ((booleanp val)
            (propertize (format "%s" val) 'face
                        (if (null val)
                            'font-lock-comment-face
                          'success)))
           ((symbolp val)
            (propertize (format "'%s" val)
                        'face 'highlight-quoted-symbol))
           ((keymapp val)
            (propertize "<keymap>" 'face 'font-lock-constant-face))
           ((listp val)
            (prin1-to-string val))
           ((stringp val)
            (propertize (format "%S" val) 'face 'font-lock-string-face))
           ((numberp val)
            (propertize (format "%s" val) 'face 'highlight-numbers-number))
           ((format "%s" val)))
     t)))


;;
;; Library

(defun +ivy--switch-buffer-preview ()
  (let (ivy-use-virtual-buffers ivy--virtual-buffers)
    (counsel--switch-buffer-update-fn)))

(defalias '+ivy--switch-buffer-preview-all #'counsel--switch-buffer-update-fn)
(defalias '+ivy--switch-buffer-unwind      #'counsel--switch-buffer-unwind)

(defun +ivy--switch-buffer (workspace other)
  (let ((current (not other))
        prompt action filter update unwind)
    (cond ((and workspace current)
           (setq prompt "Switch to workspace buffer: "
                 action #'ivy--switch-buffer-action
                 filter #'+ivy--is-workspace-other-buffer-p))
          (workspace
           (setq prompt "Switch to workspace buffer in other window: "
                 action #'ivy--switch-buffer-other-window-action
                 filter #'+ivy--is-workspace-buffer-p))
          (current
           (setq prompt "Switch to buffer: "
                 action #'ivy--switch-buffer-action))
          ((setq prompt "Switch to buffer in other window: "
                 action #'ivy--switch-buffer-other-window-action)))
    (when +ivy-buffer-preview
      (cond ((not (and ivy-use-virtual-buffers
                       (eq +ivy-buffer-preview 'everything)))
             (setq update #'+ivy--switch-buffer-preview
                   unwind #'+ivy--switch-buffer-unwind))
            ((setq update #'+ivy--switch-buffer-preview-all
                   unwind #'+ivy--switch-buffer-unwind))))
    (ivy-read prompt 'internal-complete-buffer
              :action action
              :predicate filter
              :update-fn update
              :unwind unwind
              :preselect (buffer-name (other-buffer (current-buffer)))
              :matcher #'ivy--switch-buffer-matcher
              :keymap ivy-switch-buffer-map
              ;; NOTE A clever disguise, needed for virtual buffers.
              :caller #'ivy-switch-buffer)))

;;;###autoload
(defun +ivy/switch-workspace-buffer (&optional arg)
  "Switch to another buffer within the current workspace.

If ARG (universal argument), open selection in other-window."
  (interactive "P")
  (+ivy--switch-buffer t arg))

;;;###autoload
(defun +ivy/switch-workspace-buffer-other-window ()
  "Switch another window to a buffer within the current workspace."
  (interactive)
  (+ivy--switch-buffer t t))

;;;###autoload
(defun +ivy/switch-buffer ()
  "Switch to another buffer."
  (interactive)
  (+ivy--switch-buffer nil nil))

;;;###autoload
(defun +ivy/switch-buffer-other-window ()
  "Switch to another buffer in another window."
  (interactive)
  (+ivy--switch-buffer nil t))

;;;###autoload
(defun +ivy/woccur ()
  "Invoke a wgrep buffer on the current ivy results, if supported."
  (interactive)
  (unless (window-minibuffer-p)
    (user-error "No completion session is active"))
  (require 'wgrep)
  (let ((caller (ivy-state-caller ivy-last)))
    (if-let (occur-fn (plist-get +ivy-edit-functions caller))
        (ivy-exit-with-action
         (lambda (_) (funcall occur-fn)))
      (if-let (occur-fn (plist-get ivy--occurs-list caller))
          (let ((buffer (generate-new-buffer
                         (format "*ivy-occur%s \"%s\"*"
                                 (if caller (concat " " (prin1-to-string caller)) "")
                                 ivy-text))))
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (funcall occur-fn))
              (setf (ivy-state-text ivy-last) ivy-text)
              (setq ivy-occur-last ivy-last)
              (setq-local ivy--directory ivy--directory))
            (ivy-exit-with-action
             `(lambda (_)
                (pop-to-buffer ,buffer)
                (ivy-wgrep-change-to-wgrep-mode))))
        (user-error "%S doesn't support wgrep" caller)))))

;;;###autoload
(defun +ivy-yas-prompt-fn (prompt choices &optional display-fn)
  (yas-completing-prompt prompt choices display-fn #'ivy-completing-read))

;;;###autoload
(defun +ivy-git-grep-other-window-action (x)
  "Opens the current candidate in another window."
  (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
    (select-window
     (with-ivy-window
       (let ((file-name   (match-string-no-properties 1 x))
             (line-number (match-string-no-properties 2 x)))
         (find-file-other-window (expand-file-name file-name (ivy-state-directory ivy-last)))
         (goto-char (point-min))
         (forward-line (1- (string-to-number line-number)))
         (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
         (run-hooks 'counsel-grep-post-action-hook)
         (selected-window))))))

;;;###autoload
(defun +ivy-confirm-delete-file (x)
  (dired-delete-file x 'confirm-each-subdirectory))


;;
;;; File searching

;;;###autoload
(defun +ivy/projectile-find-file ()
  "A more sensible `counsel-projectile-find-file', which will revert to
`counsel-find-file' if invoked from $HOME or /, `counsel-file-jump' if invoked
from a non-project, `projectile-find-file' if in a big project (more than
`ivy-sort-max-size' files), or `counsel-projectile-find-file' otherwise.

The point of this is to avoid Emacs locking up indexing massive file trees."
  (interactive)
  ;; Spoof the command so that ivy/counsel will display the (well fleshed-out)
  ;; actions list for `counsel-find-file' on C-o. The actions list for the other
  ;; commands aren't as well configured or are empty.
  (let ((this-command 'counsel-find-file))
    (call-interactively
     (cond ((or (file-equal-p default-directory "~")
                (file-equal-p default-directory "/")
                (when-let (proot (doom-project-root))
                  (file-equal-p proot "~")))
            #'counsel-find-file)

           ((doom-project-p)
            (let ((files (projectile-current-project-files)))
              (if (<= (length files) ivy-sort-max-size)
                  #'counsel-projectile-find-file
                #'projectile-find-file)))

           (#'counsel-file-jump)))))

;;;###autoload
(cl-defun +ivy-file-search (&key query in all-files (recursive t) prompt args)
  "Conduct a file search using ripgrep.

:query STRING
  Determines the initial input to search for.
:in PATH
  Sets what directory to base the search out of. Defaults to the current
  project's root.
:recursive BOOL
  Whether or not to search files recursively from the base directory."
  (declare (indent defun))
  (unless (executable-find "rg")
    (user-error "Couldn't find ripgrep in your PATH"))
  (require 'counsel)
  (let* ((this-command 'counsel-rg)
         (project-root (or (doom-project-root) default-directory))
         (directory (or in project-root))
         (args (concat (if all-files " -uu")
                       (unless recursive " --maxdepth 1")
                       " " (mapconcat #'shell-quote-argument args " "))))
    (setq deactivate-mark t)
    (counsel-rg
     (or query
         (when (doom-region-active-p)
           (replace-regexp-in-string
            "[! |]" (lambda (substr)
                      (cond ((and (string= substr " ")
                                  (not (featurep! +fuzzy)))
                             "  ")
                            ((string= substr "|")
                             "\\\\\\\\|")
                            ((concat "\\\\" substr))))
            (rxt-quote-pcre (doom-thing-at-point-or-region)))))
     directory args
     (or prompt
         (format "rg%s [%s]: "
                 args
                 (cond ((equal directory default-directory)
                        "./")
                       ((equal directory project-root)
                        (projectile-project-name))
                       ((file-relative-name directory project-root))))))))

;;;###autoload
(defun +ivy/project-search (&optional arg initial-query directory)
  "Performs a live project search from the project root using ripgrep.

If ARG (universal argument), include all files, even hidden or compressed ones,
in the search."
  (interactive "P")
  (+ivy-file-search :query initial-query :in directory :all-files arg))

;;;###autoload
(defun +ivy/project-search-from-cwd (&optional arg initial-query)
  "Performs a project search recursively from the current directory.

If ARG (universal argument), include all files, even hidden or compressed ones."
  (interactive "P")
  (+ivy/project-search arg initial-query default-directory))


;;
;;; Wrappers around `counsel-compile'

;;;###autoload
(defun +ivy/compile ()
  "Execute a compile command from the current buffer's directory."
  (interactive)
  (counsel-compile default-directory))

;;;###autoload
(defun +ivy/project-compile ()
  "Execute a compile command from the current project's root."
  (interactive)
  (counsel-compile (projectile-project-root)))

;;;###autoload
(defun +ivy/jump-list ()
  "Go to an entry in evil's (or better-jumper's) jumplist."
  (interactive)
  ;; REVIEW Refactor me
  (let (buffers)
    (unwind-protect
        (ivy-read "jumplist: "
                  (nreverse
                   (delete-dups
                    (delq
                     nil
                     (mapcar (lambda (mark)
                               (when mark
                                 (cl-destructuring-bind (path pt _id) mark
                                   (let ((buf (get-file-buffer path)))
                                     (unless buf
                                       (push (setq buf (find-file-noselect path t))
                                             buffers))
                                     (with-current-buffer buf
                                       (goto-char pt)
                                       (font-lock-fontify-region (line-beginning-position) (line-end-position))
                                       (cons (format "%s:%d: %s"
                                                     (buffer-name)
                                                     (line-number-at-pos)
                                                     (string-trim-right (or (thing-at-point 'line) "")))
                                             (point-marker)))))))
                             (cddr (better-jumper-jump-list-struct-ring
                                    (better-jumper-get-jumps (better-jumper--get-current-context))))))))
                  :sort nil
                  :require-match t
                  :action (lambda (cand)
                            (let ((mark (cdr cand)))
                              (delq! (marker-buffer mark) buffers)
                              (mapc #'kill-buffer buffers)
                              (setq buffers nil)
                              (with-current-buffer (switch-to-buffer (marker-buffer mark))
                                (goto-char (marker-position mark)))))
                  :caller '+ivy/jump-list)
      (mapc #'kill-buffer buffers))))

;;;###autoload
(defun +ivy/git-grep-other-window-action ()
  "Open the current counsel-{ag,rg,git-grep} candidate in other-window."
  (interactive)
  (ivy-set-action #'+ivy-git-grep-other-window-action)
  (setq ivy-exit 'done)
  (exit-minibuffer))
