;;; completion/ivy/autoload/ivy.el -*- lexical-binding: t; -*-

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
                     (not (file-in-directory-p buffer-file-truename (doom-project-root))))
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
  (let ((buf (get-buffer str)))
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


;;
;; File searching
;;

(defvar +ivy--file-last-search nil)
(defvar +ivy--file-search-recursion-p t)
(defvar +ivy--file-search-all-files-p nil)

(defun +ivy--file-search (engine &optional query directory)
  (let* ((project-root (doom-project-root))
         (directory (or directory project-root))
         (recursion-p +ivy--file-search-recursion-p)
         (all-files-p +ivy--file-search-all-files-p)
         (engine (or engine
                     (and (executable-find "rg") 'rg)
                     (and (executable-find "ag") 'ag)))
         (query
          (or query
              (when (use-region-p)
                (let ((beg (or (bound-and-true-p evil-visual-beginning) (region-beginning)))
                      (end (or (bound-and-true-p evil-visual-end) (region-end))))
                  (when (> (abs (- end beg)) 1)
                    (rxt-quote-pcre (buffer-substring-no-properties beg end)))))
              +ivy--file-last-search))
         (prompt
          (format "%s%%s %s"
                  (symbol-name engine)
                  (cond ((equal directory default-directory)
                         "./")
                        ((equal directory project-root)
                         (projectile-project-name))
                        (t
                         (file-relative-name directory project-root)))))
         (default-directory directory))
    (setq +ivy--file-last-search query)
    (require 'counsel)
    (cl-letf (((symbol-function 'counsel-ag-function)
               (symbol-function '+ivy*counsel-ag-function))
              ((symbol-function 'counsel-git-grep-function)
               (symbol-function '+ivy*counsel-git-grep-function)))
      (pcase engine
        ('grep
         (let ((args (if recursion-p " -r"))
               (counsel-projectile-grep-initial-input query)
               (default-directory directory))
           (if all-files-p
               (cl-letf (((symbol-function #'projectile-ignored-directories-rel)
                          (symbol-function #'ignore))
                         ((symbol-function #'projectile-ignored-files-rel)
                          (symbol-function #'ignore)))
                 (counsel-projectile-grep args))
             (counsel-projectile-grep args))))
        ('ag
         (let ((args (concat " -S" ; smart-case
                             (if all-files-p " -a")
                             (unless recursion-p " --depth 1"))))
           (counsel-ag query directory args (format prompt args))))
        ('rg
         (let ((args (concat (if all-files-p " -uu")
                             (unless recursion-p " --maxdepth 1"))))
           (counsel-rg query directory args (format prompt args))))
        ('pt
         (let ((counsel-pt-base-command
                (concat counsel-pt-base-command
                        " -S" ; smart-case
                        (if all-files-p " -U")
                        (unless recursion-p " --depth=1")))
               (default-directory directory))
           (counsel-pt query)))
        (_ (error "No search engine specified"))))))

;;;###autoload
(defun +ivy/project-search (arg)
  "Performs a project search using the first available search backend from a
list of: ripgrep, ag, pt, git-grep and grep. If ARG (universal argument),
preform search from current directory."
  (interactive "P")
  (call-interactively
   (cond ((executable-find "rg") (if arg #'+ivy/rg-from-cwd #'+ivy/rg))
         ((executable-find "ag") (if arg #'+ivy/ag-from-cwd #'+ivy/ag))
         ((executable-find "pt") (if arg #'+ivy/pt-from-cwd #'+ivy/pt))
         (arg #'+ivy/grep-from-cwd)
         (#'+ivy/grep))))

;;;###autoload
(defun +ivy/rg (all-files-p &optional query directory)
  "Perform a project file search using ripgrep. QUERY is a regexp. If omitted,
the current selection is used. If no selection is active, the last known search
is used.

If ALL-FILES-P, don't respect .gitignore files and search everything.

NOTE: ripgrep doesn't support multiline searches (yet)."
  (interactive "P")
  (let ((+ivy--file-search-all-files-p all-files-p))
    (+ivy--file-search 'rg query directory)))

;;;###autoload
(defun +ivy/ag (all-files-p &optional query directory)
  "Perform a project file search using the silver searcher. QUERY is a pcre
regexp. If omitted, the current selection is used. If no selection is active,
the last known search is used.

If ALL-FILES-P, don't respect .gitignore files and search everything."
  (interactive "P")
  (let ((+ivy--file-search-all-files-p all-files-p))
    (+ivy--file-search 'ag query directory)))

;;;###autoload
(defun +ivy/pt (all-files-p &optional query directory)
  "Perform a project file search using the platinum searcher. QUERY is a grep
regexp. If omitted, the current selection is used. If no selection is active,
the last known search is used.

If ALL-FILES-P, don't respect .gitignore files and search everything."
  (interactive "P")
  (let ((+ivy--file-search-all-files-p all-files-p))
    (+ivy--file-search 'pt query directory)))

;;;###autoload
(defun +ivy/grep (all-files-p &optional query directory)
  "Perform a project file search using grep (or git-grep in git repos). QUERY is
a grep regexp. If omitted, the current selection is used. If no selection is
active, the last known search is used.

If ALL-FILES-P, don't respect .gitignore files and search everything."
  (interactive "P")
  (let ((+ivy--file-search-all-files-p all-files-p))
    (+ivy--file-search 'grep query directory)))


;;;###autoload
(defun +ivy/rg-from-cwd (recursive-p &optional query)
  "Like `+ivy/rg', but from the current directory (recursively if RECURSIVE-P is
non-nil)."
  (interactive "P")
  (let ((+ivy--file-search-recursion-p recursive-p))
    (+ivy/rg t query default-directory)))

;;;###autoload
(defun +ivy/ag-from-cwd (recursive-p &optional query)
  "Like `+ivy/ag', but from the current directory (recursively if RECURSIVE-P is
non-nil)."
  (interactive "P")
  (let ((+ivy--file-search-recursion-p recursive-p))
    (+ivy/ag t query default-directory)))

;;;###autoload
(defun +ivy/pt-from-cwd (recursive-p &optional query)
  "Like `+ivy/pt', but from the current directory (recursively if RECURSIVE-P is
non-nil)."
  (interactive "P")
  (let ((+ivy--file-search-recursion-p recursive-p))
    (+ivy/pt t query default-directory)))

;;;###autoload
(defun +ivy/grep-from-cwd (recursive-p &optional query)
  "Like `+ivy/grep', but from the current directory (recursively if RECURSIVE-P is
non-nil)."
  (interactive "P")
  (let ((+ivy--file-search-recursion-p recursive-p))
    (+ivy/grep t query default-directory)))


;;
;; Advice
;;

;;;###autoload
(defun +ivy*counsel-ag-function (string)
  "Advice to get rid of the character limit from `counsel-ag-function'.

NOTE This may need to be updated frequently, to meet changes upstream (in
counsel-rg)."
  (if (< (length string) 1)  ; <-- modified the character limit
      (counsel-more-chars 1) ; <--
    (let ((default-directory (ivy-state-directory ivy-last))
          (regex (counsel-unquote-regex-parens
                  (setq ivy--old-re
                        (ivy--regex string)))))
      (counsel--async-command (format counsel-ag-command
                                      (shell-quote-argument regex)))
      nil)))

;;;###autoload
(defun +ivy*counsel-git-grep-function (string)
  "Advice to get rid of the character limit from `counsel-git-grep-function'.

NOTE This may need to be updated frequently, to meet changes upstream (in
counsel-git-grep)."
  (if (and (> counsel--git-grep-count counsel--git-grep-count-threshold)
           (< (length string) 1)) ; <-- modified the character limit
      (counsel-more-chars 1)      ; <--
    (let* ((default-directory (ivy-state-directory ivy-last))
           (cmd (format counsel-git-grep-cmd
                        (setq ivy--old-re (ivy--regex string t)))))
      (if (<= counsel--git-grep-count counsel--git-grep-count-threshold)
          (split-string (shell-command-to-string cmd) "\n" t)
        (counsel--gg-candidates (ivy--regex string))
        nil))))
