;;; ui/workspaces/autoload/workspaces.el -*- lexical-binding: t; -*-


;;
;;; Public

;;;###autoload
(defmacro with-current-workspace (ws &rest body)
  "Evaluate BODY while WS is the active workspace."
  (declare (indent defun))
  `(let ((--tab-- ,ws)
         (--last-tab-- (+workspaces-current-name)))
     (tab-bar-select-tab-by-name (alist-get 'name --tab--))
     (unwind-protect
         (progn
           ,@body)
       (tab-bar-select-tab-by-name --last-tab--))))

;;;###autoload
(defmacro with-workspace-buffers (&rest body)
  "Evaluate BODY with the buffer list scoped to the current workspace."
  `(if (bound-and-true-p tabspaces-mode)
       (letf! ((#'buffer-list #'tabspaces--buffer-list))
         ,@body)
     ,@body))

;;;###autoload
(cl-defun +workspaces-list (&optional (frames t))
  "Return all open tabs in FRAMES (defaults to current frame)."
  (let (workspaces)
    (dolist (fr (if (eq frames t) (list (selected-frame)) frames))
      (dolist (tab (tab-bar-tabs fr))
        (push (if (eq (car tab) 'current-tab)
                  (tab-bar--tab fr)
                tab)
              workspaces)))
    (nreverse workspaces)))

;;;###autoload
(defun +workspaces-buffer-list (&rest tabs)
  "Return a list of buffers associated with TAB."
  (seq-filter
   #'buffer-live-p
   (if tabs
       (cl-remove-duplicates
        (cl-loop for tab in (delq nil tabs)
                 for ws = (if (numberp tab) (nth tab (tab-bar-tabs)) tab)
                 if (eq (car-safe ws) 'current-tab)
                 append (append (frame-parameter nil 'buffer-list)
                                (frame-parameter nil 'bured-buffer-list))
                 else
                 append (append (alist-get 'wc-bl (cdr ws))
                                (alist-get 'wc-bbl (cdr ws))))
        :test #'eq)
     (append (frame-parameter nil 'buffer-list)
             (frame-parameter nil 'buried-buffer-list)
             nil))))

;;;###autoload
(defun +workspaces-other-buffer-list ()
  "Return a list of buffers in all other workspaces except the current one."
  (let ((buffers (buffer-list)))
    (dolist (b (+workspaces-buffer-list))
      (cl-callf2 delete b buffers))
    buffers))

;;;###autoload
(cl-defun +workspaces-contain-buffer-p
    (buffer &optional (tab t) (buffer-list (+workspaces-buffer-list tab)))
  "Return non-nil if BUFFEr is in TAB."
  (memq buffer buffer-list))

;;;###autoload
(defun +workspaces-get (name &optional frame noerror?)
  "Return a full tab data structure for NAME in FRAME.

NAME can be a string representing a named tab or a number representing it's
index. If NOERROR? is omitted, throws an error if NAME doesn't exist."
  (if-let* ((idx (if (numberp name)
                     name
                   (tab-bar--tab-index-by-name name nil frame)))
            (tab (nth idx (funcall tab-bar-tabs-function frame))))
      (if (eq (car tab) 'current-tab)
          (let (tab-bar-select-restore-context)
            (tab-bar--tab frame))
        tab)
    (unless noerror?
      (user-error "No workspace found: %s" name))))

;;;###autoload
(defalias '+workspaces-exists-p #'tab-bar--tab-index-by-name)

;;;###autoload
(defun +workspaces-current ()
  "Return the current workspace."
  (tab-bar--current-tab-find (tab-bar-tabs)))

;;;###autoload
(defun +workspaces-current-name ()
  "Return the name of the current workspace."
  (alist-get 'name (+workspaces-current)))

;;;###autoload
(defun +workspaces-switch (name &optional required?)
  "Switch to a workspace named NAME or create it.

If REQUIRED? is non-nil, throw an error instead of auto-creating a non-existent
workspace."
  (let ((workspaces (mapcar (lambda (tab)
                              (alist-get 'name tab))
                            (tab-bar-tabs))))
    (unless (member name workspaces)
      (when required?
        (user-error "No workspace: %s" name)))
    (tab-bar-select-tab-by-name name)))

;;;###autoload
(defun +workspaces-add-buffer-to-tab (buffer tab)
  "Add BUFFER to TAB."
  (cl-check-type buffer buffer)
  (let ((tab-names (mapcar
                    (lambda (tab) (alist-get 'name tab))
                    (funcall tab-bar-tabs-function))))
    ;; add buffer to default tabspace
    (tab-bar-select-tab-by-name (alist-get 'name tab))
    (display-buffer buffer)
    (switch-to-buffer buffer t nil)
    (if (one-window-p t)
        (previous-buffer)
      (delete-window))
    (tab-bar-switch-to-recent-tab)))

;;;###autoload
(defun +workspaces-remove-buffer-from-tab (buffer &optional tab)
  "Remove BUFFER from TAB."
  (cl-check-type buffer buffer)
  (if tab
      (with-current-workspace tab
        (tabspaces-remove-buffer buffer))
    (tabspaces-remove-buffer buffer)))


;;
;;; Interactive commands

;;;###autoload
(defalias '+workspaces/new #'tab-bar-new-tab)

;;;###autoload
(defalias '+workspaces/new-named #'tabspaces-switch-or-create-workspace)

;;;###autoload
(defalias '+workspaces/save-session #'tabspaces-save-session)

;;;###autoload
(defalias '+workspaces/restore-last-session #'tabspaces-restore-session)

;;;###autoload
(defalias '+workspaces/kill-other #'tab-bar-close-other-tabs)

;;;###autoload
(defun +workspaces/kill (tab-index)
  "Kill workspace a TAB-INDEX (zero-based) and kill its exclusive buffers."
  (interactive (list (tab-bar--current-tab-index)))
  (unless tab-index
    (user-error "No tab number specified"))
  (let* ((current-idx (tab-bar--current-tab-index))
         (tab-idx (or tab-index current-idx))
         (tab-buffers (+workspaces-buffer-list tab-idx))
         (other-buffers
          (delete-dups
           (cl-loop for ws in (tab-bar-tabs)
                    unless (eq (car ws) 'current-tab)
                    append (+workspaces-buffer-list ws)))))
    (unwind-protect
        (cl-loop for b in tab-buffers
                 unless (member b other-buffers)  ; only kill if not open elsewhere
                 when (buffer-live-p b)
                 do (kill-buffer b))
      (tab-bar-close-tab (1+ tab-idx)))))

;;;###autoload
(defalias '+workspaces/rename #'tab-bar-rename-tab)

;;;###autoload
(defun +workspaces/kill-session ()
  "Delete the current session, all workspaces, windows and their buffers."
  (interactive)
  (tab-bar-close-other-tabs)
  (doom/kill-all-buffers (buffer-list))
  (tabspaces-reset-buffer-list)
  (switch-to-buffer (doom-fallback-buffer)))

;;;###autoload
(defalias '+workspaces/undo-close #'tab-bar-undo-close-tab)

;;;###autoload
(defun +workspaces/switch-to (index-or-name)
  "Switch to a workspace at a given INDEX-OR-NAME.
A negative number will start from the end of the workspace list."
  (interactive
   (list (or current-prefix-arg
             (completing-read "Switch to workspace: " (tabspaces--list-tabspaces)))))
  (if (numberp index-or-name)
      (tab-bar-select-tab (1+ index-or-name))
    (tabspaces-switch-or-create-workspace
     (or (cl-loop for tab in (tab-bar-tabs)
                  for name = (alist-get 'name tab)
                  if (equal name index-or-name)
                  return name)
         (user-error "No workspace with name: %s" name)))))

;;;###autoload
(dotimes (i 9)
  (defalias (intern (format "+workspaces/switch-to-%d" i))
    (cmd! (+workspaces/switch-to i))))

;;;###autoload
(defalias '+workspaces/switch-to-final #'tab-bar-switch-to-last-tab)

;;;###autoload
(defun +workspaces/close-window-or-workspace ()
  "Close the selected window, tab, or frame.

If it's the last window in the workspace, either close the workspace (as well as
its associated frame, if one exists) and move to the next."
  (interactive)
  (cond ((or (window-parent)
             (window-dedicated-p)
             (not (bound-and-true-p tabspaces-mode)))
         (funcall (if (featurep 'evil) #'evil-window-delete #'delete-window)))
        ((cdr (tab-bar-tabs))
         (call-interactively #'+workspaces/kill))
        ((user-error "Can't delete last workspace"))))

;;;###autoload
(defun +workspaces/open-in-project (project &optional force?)
  "Open an existing or new workspace for PROJECT.

Afterwards, executes `+workspaces-switch-project-function', if set.

If FORCE?, always create a workspace, even if it already exists."
  (interactive
   (list (if-let* ((projects (projectile-relevant-known-projects)))
             (projectile-completing-read "Switch to project: " projects)
           (user-error "There are no known projects"))
         current-prefix-arg))
  (let* ((project (expand-file-name project))
         (existing-tab-names (tabspaces--list-tabspaces))
         (original-tab-name (or (cdr (assoc project tabspaces-project-tab-map))
                                (tabspaces-generate-descriptive-tab-name project existing-tab-names)))
         (tab-name original-tab-name)
         (session (tabspaces--get-project-session-file-for-restore project))
         (project-directory project)  ; Use the full path as the project directory
         (project-exists (cl-member project projectile-known-projects :test #'file-equal-p))
         (create-new-tab (or force? (not (member tab-name existing-tab-names)))))

    (with-current-buffer (doom-fallback-buffer)
      (setq-local default-directory project-directory))

    (cond
     ;; If there is no tab nor project, create both
     ((not project-exists)
      (message "Creating new workspace for project...")
      (tab-bar-new-tab)
      (tab-bar-rename-tab tab-name)
      (projectile-add-known-project project-directory)
      (switch-to-buffer (doom-fallback-buffer)))

     ;; If project and tab exist, but we want a new tab
     ((and project-exists
           (member tab-name existing-tab-names)
           create-new-tab)
      (message "Creating new workspace for known project...")
      (let ((new-tab-name (generate-unique-numbered-tab-name tab-name existing-tab-names)))
        (tab-bar-new-tab)
        (tab-bar-rename-tab new-tab-name)
        (setq tab-name new-tab-name)
        (switch-to-buffer (doom-fallback-buffer))))

     ;; If project and tab exist, switch to it
     ((and project-exists
           (member tab-name existing-tab-names))
      (message "Switching to existing project workspace...")
      (tab-bar-switch-to-tab tab-name))

     ;; If project exists, but no corresponding tab, open a new tab
     (project-exists
      (message "Creating new workspace for existing project...")
      (tab-bar-new-tab)
      (tab-bar-rename-tab tab-name)
      (if (file-exists-p session)
          (tabspaces-restore-session session)
        (switch-to-buffer (doom-fallback-buffer))))

     ((user-error "Failed to find the project or create a workspace")))

    ;; Update tabspaces-project-tab-map (only for the main tab, not numbered duplicates)
    (unless (string-match-p "<[0-9]+>$" tab-name)
      (setf (alist-get project-directory tabspaces-project-tab-map nil nil #'equal)
            tab-name))))

;;; workspaces.el ends here
