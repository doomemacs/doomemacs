;;; completion/ivy/autoload/ivy.el -*- lexical-binding: t; -*-

(defvar doom--project-root nil)

(defun +ivy--is-workspace-or-other-buffer-p (buffer)
  (let ((buffer (car buffer)))
    (when (stringp buffer)
      (setq buffer (get-buffer buffer)))
    (and (not (eq buffer (current-buffer)))
         (+workspace-contains-buffer-p buffer))))

(defun +ivy*rich-switch-buffer-buffer-name (str)
  (propertize
   (ivy-rich-switch-buffer-pad str ivy-rich-switch-buffer-name-max-length)
   'face (cond ((string-match-p "^ *\\*" str)
                'font-lock-comment-face)
               ((and buffer-file-truename
                     (not (file-in-directory-p buffer-file-truename doom--project-root)))
                'font-lock-doc-face)
               (t nil))))
(advice-add 'ivy-rich-switch-buffer-buffer-name :override #'+ivy*rich-switch-buffer-buffer-name)


;;
;; Library
;;

;;;###autoload
(defun +ivy-projectile-find-file-transformer (str)
  "Highlight entries that have been visited. This is the opposite of
`counsel-projectile-find-file'."
  (cond ((get-file-buffer (projectile-expand-root str))
         (propertize str 'face '(:weight ultra-bold :slant italic)))
        (t str)))

;;;###autoload
(defun +ivy-recentf-transformer (str)
  "Dim recentf entries that are not in the current project of the buffer you
started `counsel-recentf' from. Also uses `abbreviate-file-name'."
  (let ((str (abbreviate-file-name str)))
    (if (file-in-directory-p str (doom-project-root))
        str
      (propertize str 'face 'ivy-virtual))))

;;;###autoload
(defun +ivy-buffer-transformer (str)
  "Dim special buffers, buffers whose file aren't in the current buffer, and
virtual buffers. Uses `ivy-rich' under the hood."
  (let ((buf (get-buffer str))
        (doom--project-root (doom-project-root)))
    (require 'ivy-rich)
    (cond (buf (ivy-rich-switch-buffer-transformer str))
          ((and (eq ivy-virtual-abbreviate 'full)
                ivy-rich-switch-buffer-align-virtual-buffer)
           (ivy-rich-switch-buffer-virtual-buffer str))
          ((eq ivy-virtual-abbreviate 'full)
           (propertize (abbreviate-file-name str) 'str 'ivy-virtual))
          (t (propertize str 'face 'ivy-virtual)))))

;;;###autoload
(defun +ivy/switch-workspace-buffer (&optional arg)
  "Switch to another buffer within the current workspace.

If ARG (universal argument), open selection in other-window."
  (interactive "P")
  (ivy-read "Switch to workspace buffer: "
            'internal-complete-buffer
            :predicate #'+ivy--is-workspace-or-other-buffer-p
            :action (if arg
                        #'ivy--switch-buffer-other-window-action
                      #'ivy--switch-buffer-action)
            :matcher #'ivy--switch-buffer-matcher
            :keymap ivy-switch-buffer-map
            :caller #'+ivy/switch-workspace-buffer))

(defun +ivy--tasks-candidates (tasks)
  "Generate a list of task tags (specified by `+ivy-task-tags') for
`+ivy/tasks'."
  (let* ((max-type-width
          (cl-loop for task in +ivy-task-tags maximize (length (car task))))
         (max-desc-width
          (cl-loop for task in tasks maximize (length (cl-cdadr task))))
         (max-width (max (- (frame-width) (1+ max-type-width) max-desc-width)
                         25)))
    (cl-loop
     with fmt = (format "%%-%ds %%-%ds%%s%%s:%%s" max-type-width max-width)
     for alist in tasks
     collect
     (let-alist alist
       (format fmt
               (propertize .type 'face (cdr (assoc .type +ivy-task-tags)))
               (substring .desc 0 (min max-desc-width (length .desc)))
               (propertize " | " 'face 'font-lock-comment-face)
               (propertize (abbreviate-file-name .file) 'face 'font-lock-keyword-face)
               (propertize .line 'face 'font-lock-constant-face))))))

(defun +ivy--tasks (target)
  (let* (case-fold-search
         (task-tags (mapcar #'car +ivy-task-tags))
         (cmd
          (format "%s -H -S --no-heading -- %s %s"
                  (or (when-let* ((bin (executable-find "rg")))
                        (concat bin " --line-number"))
                      (when-let* ((bin (executable-find "ag")))
                        (concat bin " --numbers"))
                      (error "ripgrep & the_silver_searcher are unavailable"))
                  (shell-quote-argument
                   (concat "\\s("
                           (string-join task-tags "|")
                           ")([\\s:]|\\([^)]+\\):?)"))
                  target)))
    (save-match-data
      (cl-loop with out = (shell-command-to-string cmd)
               for x in (and out (split-string out "\n" t))
               when (condition-case-unless-debug ex
                      (string-match
                       (concat "^\\([^:]+\\):\\([0-9]+\\):.+\\("
                               (string-join task-tags "\\|")
                               "\\):?\\s-*\\(.+\\)")
                       x)
                      (error
                       (message! (red "Error matching task in file: (%s) %s"
                                      (error-message-string ex)
                                      (car (split-string x ":"))))
                       nil))
               collect `((type . ,(match-string 3 x))
                         (desc . ,(match-string 4 x))
                         (file . ,(match-string 1 x))
                         (line . ,(match-string 2 x)))))))

(defun +ivy--tasks-open-action (x)
  "Jump to the file and line of the current task."
  (let ((location (cadr (split-string x " | ")))
        (type (car (split-string x " "))))
    (cl-destructuring-bind (file line) (split-string location ":")
      (with-ivy-window
        (find-file (expand-file-name file (doom-project-root)))
        (goto-char (point-min))
        (forward-line (1- (string-to-number line)))
        (search-forward type (line-end-position) t)
        (backward-char (length type))
        (recenter)))))

;;;###autoload
(defun +ivy/tasks (&optional arg)
  "Search through all TODO/FIXME tags in the current project. If ARG, only
search current file. See `+ivy-task-tags' to customize what this searches for."
  (interactive "P")
  (ivy-read (format "Tasks (%s): "
                    (if arg
                        (concat "in: " (file-relative-name buffer-file-name))
                      "project"))
            (+ivy--tasks-candidates
             (+ivy--tasks (if arg buffer-file-name (doom-project-root))))
            :action #'+ivy--tasks-open-action
            :caller '+ivy/tasks))

;;;###autoload
(defun +ivy*counsel-ag-function (string base-cmd extra-ag-args)
  "Advice to 1) get rid of the character limit from `counsel-ag-function' and 2)
disable ivy's over-zealous parentheses quoting behavior (if i want literal
parentheses, I'll escape them myself).

NOTE This may need to be updated frequently, to meet changes upstream (in
counsel-rg)."
  (when (null extra-ag-args)
    (setq extra-ag-args ""))
  (if (< (length string) 1)  ; <-- modified the character limit
      (counsel-more-chars 1) ; <--
    (let ((default-directory (ivy-state-directory ivy-last))
          (regex (counsel-unquote-regex-parens
                  (setq ivy--old-re
                        (ivy--regex string)))))
      (let* ((args-end (string-match " -- " extra-ag-args))
             (file (if args-end
                       (substring-no-properties extra-ag-args (+ args-end 3))
                     ""))
             (extra-ag-args (if args-end
                                (substring-no-properties extra-ag-args 0 args-end)
                              extra-ag-args))
             (ag-cmd (format base-cmd
                             (concat extra-ag-args
                                     " -- "
                                     (shell-quote-argument regex)
                                     file))))
        (counsel--async-command ag-cmd)
        nil))))

;;;###autoload
(defun +ivy/wgrep-occur ()
  "Invoke the search+replace wgrep buffer on the current ag/rg search results."
  (interactive)
  (unless (window-minibuffer-p)
    (user-error "No completion session is active"))
  (require 'wgrep)
  (let* ((caller (ivy-state-caller ivy-last))
         (occur-fn (plist-get ivy--occurs-list caller))
         (buffer
          (generate-new-buffer
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
        (ivy-wgrep-change-to-wgrep-mode)))))

;;;###autoload
(defun +ivy-yas-prompt (prompt choices &optional display-fn)
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
