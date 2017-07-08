;;; feature/workspaces/autoload/workspaces.el -*- lexical-binding: t; -*-

(defvar +workspace-workspace-file "_workspaces"
  "The file basename in which to store single workspace perspectives.")

(defvar +workspace--last nil)

(defface +workspace-tab-selected-face
  '((t (:inherit 'highlight)))
  "The face for selected tabs displayed by `+workspace/display'"
  :group 'doom)

(defface +workspace-tab-face
  '((t (:inherit 'default)))
  "The face for selected tabs displayed by `+workspace/display'"
  :group 'doom)

;;;###autoload
(defun +workspace-list ()
  "Return a list of workspace structs."
  (mapcar #'persp-get-by-name (+workspace-list-names)))

;;;###autoload
(defun +workspace-list-names ()
  "Return a list of workspace names (strings)."
  (delete persp-nil-name (persp-names-current-frame-fast-ordered)))

;;;###autoload
(defun +workspace-buffer-list (&optional persp)
  "Return a list of buffers in PERSP (defaults to the current perspective).

The buffer list is ordered by recency (same as `buffer-list').

PERSP can be a string (name of a workspace) or a perspective hash (satisfies
`+workspace-p').

If PERSP is t, then return a list of orphaned buffers associated with no
perspectives."
  (unless persp
    (setq persp (get-current-persp)))
  (if (eq persp t)
      (cl-remove-if #'persp--buffer-in-persps (buffer-list))
    (when (stringp persp)
      (setq persp (+workspace-get persp t)))
    (cl-loop for buf in (buffer-list)
             if (persp-contain-buffer-p buf persp)
             collect buf)))

;;;###autoload
(defun +workspace-p (obj)
  "Return t if OBJ is a perspective hash table."
  (and obj
       (cl-struct-p obj)
       (perspective-p obj)))

;;;###autoload
(defun +workspace-exists-p (name)
  "Returns t if NAME is the name of an existing workspace."
  (when (symbolp name)
    (setq name (symbol-name name)))
  (unless (stringp name)
    (error "Expected a string, got a %s" (type-of name)))
  (member name (+workspace-list-names)))

;;;###autoload
(defun +workspace-get (name &optional noerror)
  "Returns a workspace (perspective hash table) named NAME."
  (unless (equal name persp-nil-name)
    (let ((persp (persp-get-by-name name)))
      (when (and (not noerror)
                 (or (null persp)
                     (equal persp persp-not-persp)))
        (error "%s is not an available workspace" name))
      persp)))

;;;###autoload
(defalias '+workspace-current #'get-current-persp)

;;;###autoload
(defun +workspace-current-name ()
  "Get the name of the currently active workspace."
  (safe-persp-name (get-current-persp)))

;;;###autoload
(defun +workspace-contains-buffer-p (&optional buffer workspace)
  "Return non-nil if buffer is in workspace (defaults to current workspace)."
  (unless workspace
    (setq workspace (+workspace-current)))
  (persp-contain-buffer-p buffer workspace nil))

;;;###autoload
(defun +workspace-load (name)
  "Loads and inserts a single workspace (named NAME) into the current session.
Can only retrieve perspectives that were explicitly saved with
`+workspace-save'.

Returns t if successful, nil otherwise."
  (unless (+workspace-exists-p name)
    (persp-load-from-file-by-names +workspace-workspace-file *persp-hash* (list name)))
  (+workspace-exists-p name))

;;;###autoload
(defun +workspace-load-session (&optional name)
  "Replace current session with the entire session named NAME. If NAME is nil,
use `persp-auto-save-fname'."
  (persp-load-state-from-file (or name persp-auto-save-fname)))

;;;###autoload
(defun +workspace-save (name)
  "Saves a single workspace (TARGET) from the current session. Can be loaded
again with `+workspace-load'. TARGET can be the string name of a workspace or
its perspective hash table.

Returns t on success, nil otherwise."
  (unless (+workspace-exists-p name)
    (error "%s is not an available workspace" name))
  (persp-save-to-file-by-names
   +workspace-workspace-file *persp-hash* (list name) t)
  (memq name (persp-list-persp-names-in-file +workspace-workspace-file)))

;;;###autoload
(defun +workspace-save-session (&optional name)
  "Save a whole session as NAME. If NAME is nil, use `persp-auto-save-fname'.
Return t on success, nil otherwise."
  (when (or (not name)
            (equal name persp-auto-save-fname))
    (setq name persp-auto-save-fname
          persp-auto-save-opt 0))
  (and (persp-save-state-to-file name) t))

;;;###autoload
(defun +workspace-new (name)
  "Create a new workspace named NAME. If one already exists, return nil.
Otherwise return t on success, nil otherwise."
  (when (+workspace-protected-p name)
    (error "Can't create a new '%s' workspace" name))
  (unless (+workspace-exists-p name)
    (and (persp-add-new name) t)))

;;;###autoload
(defun +workspace-rename (name new-name)
  "Rename the current workspace named NAME to NEW-NAME. Returns old name on
success, nil otherwise."
  (when (+workspace-protected-p name)
    (error "Can't rename '%s' workspace" name))
  (persp-rename new-name (+workspace-get name)))

;;;###autoload
(defun +workspace-delete (name &optional inhibit-kill-p)
  "Delete the workspace denoted by TARGET, which can be the name of a
perspective or its hash table."
  (when (+workspace-protected-p name)
    (error "Can't delete '%s' workspace" name))
  (+workspace-get name) ;; error checking
  (persp-kill name inhibit-kill-p))

;;;###autoload
(defun +workspace-switch (name &optional auto-create-p)
  "Switch to another workspace."
  (unless (+workspace-exists-p name)
    (if auto-create-p
        (+workspace-new name)
      (error "%s is not an available workspace" name)))
  (let ((old-name (+workspace-current-name)))
    (setq +workspace--last
          (or (and (not (string= old-name persp-nil-name))
                   old-name)
              +workspaces-main)))
  (persp-frame-switch name))

(defun +workspace--generate-id ()
  (or (cl-loop for name in (+workspace-list-names)
               when (string-match-p "^#[0-9]+$" name)
               maximize (string-to-number (substring name 1)) into max
               finally return (if max (1+ max)))
      1))

(defun +workspace-protected-p (name)
  (or (equal name persp-nil-name)
      (equal name +workspaces-main)))


;;
;; Interactive commands
;;

;;;###autoload
(defun +workspace/load (name)
  "Load a workspace and switch to it. If called with C-u, try to reload the
current workspace (by name) from session files."
  (interactive
   (list
    (if current-prefix-arg
        (+workspace-current-name)
      (completing-read "Workspace to load: "
                       (persp-list-persp-names-in-file
                        +workspace-workspace-file)))))
  (if (not (+workspace-load name))
      (+workspace-error (format "Couldn't load workspace %s" name))
    (+workspace/switch-to name)
    (+workspace/display)))

;;;###autoload
(defun +workspace/load-session (&optional name)
  "Load a session and switch to it. If called with C-u, try to load the last
session."
  (interactive
   (list
    (unless current-prefix-arg
      (completing-read
       "Session to load: "
       (directory-files persp-save-dir nil "^[^_.]")
       nil t))))
  (condition-case ex
      (let ((name (or name persp-auto-save-fname)))
        (+workspace-load-session name)
        (+workspace-message (format "'%s' workspace loaded" name) 'success))
    '(error (+workspace-error (cadr ex) t))))

;;;###autoload
(defun +workspace/save (name)
  "Save the current workspace. If called with C-u, autosave the current
workspace."
  (interactive
   (list
    (if current-prefix-arg
        (+workspace-current-name)
      (completing-read "Workspace to save: " (+workspace-list-names)))))
  (if (+workspace-save name)
      (+workspace-message (format "'%s' workspace saved" name) 'success)
    (+workspace-error (format "Couldn't save workspace %s" name))))

;;;###autoload
(defun +workspace/save-session (&optional name)
  "Save the current session. If called with C-u, prompt you for the name to save
the session as."
  (interactive
   (list
    (when current-prefix-arg
      (completing-read
       "Save session as: "
       (directory-files persp-save-dir nil "^[^_.]")))))
  (condition-case ex
      (let ((name (or name persp-auto-save-fname)))
        (if (+workspace-save-session name)
            (+workspace-message (format "Saved session as '%s'" name) 'success)
          (error "Couldn't save session as '%s'" name)))
    '(error (+workspace-error (cadr ex) t))))

;;;###autoload
(defun +workspace/rename (new-name)
  "Rename the current workspace."
  (interactive)
  (condition-case ex
      (let* ((current-name (+workspace-current-name))
             (old-name (+workspace-rename current-name new-name)))
        (unless old-name
          (error "Failed to rename %s" current-name))
        (+workspace-message (format "Renamed '%s'->'%s'" old-name new-name) 'success))
    ('error (+workspace-error (cadr ex) t))))

;;;###autoload
(defun +workspace/delete (name)
  "Delete this workspace. If called with C-u, prompts you for the name of the
workspace to delete."
  (interactive
   (let ((current-name (+workspace-current-name)))
     (list
      (if current-prefix-arg
          (completing-read (format "Delete workspace (default: %s): " current-name)
                           (+workspace-list-names)
                           nil nil current-name)
        current-name))))
  (condition-case ex
      (if (not (+workspace-delete name))
          (error "Couldn't delete %s workspace" name)
        (if (+workspace-exists-p +workspace--last)
            (+workspace-switch +workspace--last)
          (+workspace-switch +workspaces-main t))
        (+workspace-message (format "Deleted '%s' workspace" name) 'success))
    ('error (+workspace-error (cadr ex) t))))

;;;###autoload
(defun +workspace/kill-session ()
  "Delete the current session, clears all workspaces, windows and buffers."
  (interactive)
  (unless (cl-every #'+workspace-delete
                    (delete +workspaces-main (+workspace-list-names)))
    (+workspace-error "Could not clear session"))
  (+workspace-switch +workspaces-main t)
  (doom/kill-all-buffers)
  (let ((fallback-buf (doom-fallback-buffer)))
    (switch-to-buffer fallback-buf)
    (doom/cleanup-buffers)))

;;;###autoload
(defun +workspace/kill-session-and-quit ()
  "Forgets current session and quits."
  (+workspace/kill-session)
  (save-buffers-kill-terminal))

;;;###autoload
(defun +workspace/new (&optional name clone-p)
  "Create a new workspace named NAME. If OVERWRITE-P is non-nil, clear any
pre-existing workspace."
  (interactive "iP")
  (unless name
    (setq name (format "#%s" (+workspace--generate-id))))
  (condition-case ex
      (let ((exists-p (+workspace-exists-p name)))
        (if exists-p
            (error "%s already exists" name)
          (+workspace-switch name t)
          (if clone-p
              (dolist (window (window-list))
                (persp-add-buffer (window-buffer window) persp nil))
            (delete-other-windows-internal)
            (switch-to-buffer (doom-fallback-buffer)))
          (+workspace/display)))
    ('error (+workspace-error (cadr ex) t))))

;;;###autoload
(defun +workspace/switch-to (index)
  "Switch to a workspace at a given INDEX. A negative number will start from the
end of the workspace list."
  (interactive
   (list (or current-prefix-arg
             (completing-read "Switch to workspace: " (+workspace-list-names)))))
  (when (and (stringp index)
             (string-match-p "^[0-9]+$" index))
    (setq index (string-to-number index)))
  (condition-case ex
      (let ((names (+workspace-list-names))
            (old-name (+workspace-current-name)))
        (cond ((numberp index)
               (let ((dest (nth index names)))
                 (unless dest
                   (error "No workspace at #%s" (1+ index)))
                 (+workspace-switch dest)))
              ((stringp index)
               (unless (member index names)
                 (error "No workspace named %s" index))
               (+workspace-switch index))
              (t
               (error "Not a valid index: %s" index)))
        (unless (called-interactively-p 'interactive)
          (if (equal (+workspace-current-name) old-name)
              (+workspace-message (format "Already in %s" old-name) 'warn)
            (+workspace/display))))
    ('error (+workspace-error (cadr ex) t))))

;;;###autoload
(defun +workspace/switch-to-last ()
  "Switch to the last workspace."
  (interactive)
  (+workspace/switch-to (car (last (+workspace-list-names)))))

;;;###autoload
(defun +workspace/cycle (n)
  "Cycle n workspaces to the right (default) or left."
  (interactive (list 1))
  (let ((current-name (+workspace-current-name)))
    (if (equal current-name persp-nil-name)
        (+workspace-switch +workspaces-main t)
      (condition-case ex
          (let* ((persps (+workspace-list-names))
                 (perspc (length persps))
                 (index (cl-position current-name persps)))
            (when (= perspc 1)
              (user-error "No other workspaces"))
            (+workspace/switch-to (% (+ index n) perspc))
            (unless (called-interactively-p 'interactive)
              (+workspace/display)))
        ('user-error (+workspace-error (cadr ex) t))
        ('error (+workspace-error ex t))))))

;;;###autoload
(defun +workspace/switch-left ()  (interactive) (+workspace/cycle -1))

;;;###autoload
(defun +workspace/switch-right () (interactive) (+workspace/cycle +1))

;;;###autoload
(defun +workspace/close-window-or-workspace ()
  "Close the selected window. If it's the last window in the workspace, close
the workspace and move to the next."
  (interactive)
  (if (doom-popup-p)
      (doom/popup-close)
    (let ((current-persp-name (+workspace-current-name)))
      (cond ((or (+workspace-protected-p current-persp-name)
                 (> (length (doom-visible-windows)) 1))
             (if (bound-and-true-p evil-mode)
                 (evil-window-delete)
               (delete-window)))
            ((> (length (+workspace-list-names)) 1)
             (+workspace/delete current-persp-name))))))

;;;###autoload
(defun +workspace/cleanup ()
  "Clean up orphaned buffers and processes."
  (interactive)
  (let ((buffers (cl-remove-if #'persp--buffer-in-persps (buffer-list)))
        (n (doom-kill-process-buffers)))
    (mapc #'kill-buffer buffers)
    (when (called-interactively-p 'any)
      (message "Cleaned up %d buffers and %d processes"
               (length buffers) n))))


;;
;; Tabs display in minibuffer
;;

(defun +workspace--tabline (&optional names)
  (let ((names (or names (+workspace-list-names)))
        (current-name (+workspace-current-name)))
    (mapconcat
     #'identity
     (cl-loop for name in names
              for i to (length names)
              collect
              (propertize (format " [%d] %s " i name)
                          'face (if (equal current-name name)
                                    '+workspace-tab-selected-face
                                  '+workspace-tab-face)))
     " ")))

(defun +workspace--message-body (message &optional  type)
  (concat (+workspace--tabline)
          (propertize " | " 'face 'font-lock-comment-face)
          (propertize (format "%s" message)
                      'face (pcase type
                              ('error 'error)
                              ('warn 'warning)
                              ('success 'success)
                              ('info 'font-lock-comment-face)))))

;;;###autoload
(defun +workspace-message (message &optional type)
  "Show an 'elegant' message in the echo area next to a listing of workspaces."
  (message "%s" (+workspace--message-body message type)))

;;;###autoload
(defun +workspace-error (message &optional noerror)
  "Show an 'elegant' error in the echo area next to a listing of workspaces."
  (funcall (if noerror #'message #'error) "%s" (+workspace--message-body message 'error)))

;;;###autoload
(defun +workspace/display ()
  "Display a list of workspaces (like tabs) in the echo area."
  (interactive)
  (message "%s" (+workspace--tabline)))

;;;###autoload
(defun +workspace-on-new-frame (frame &optional _new-frame-p)
  "Spawn a perspective for each new frame."
  (select-frame frame)
  (+workspace/new)
  (set-frame-parameter frame 'assoc-persp (+workspace-current-name)))

;;;###autoload
(defun +workspaces|create-project-workspace ()
  "Create a new workspace when switching project with `projectile'."
  (when persp-mode
    (+workspace-switch (projectile-project-name) t)))

;;;###autoload
(defun +workspaces|delete-associated-workspace-maybe (frame)
  "Delete workspace associated with current frame IF it has no real buffers."
  (when persp-mode
    (let ((frame-persp (frame-parameter frame 'assoc-persp)))
      (when (and (equal frame-persp (+workspace-current-name))
                 (not (equal frame-persp +workspaces-main)))
        (+workspace/delete frame-persp)))))

;;;###autoload
(defun +workspaces*autosave-real-buffers (orig-fn &rest args)
    "Don't autosave if no real buffers are open."
    (when (doom-real-buffer-list)
      (apply orig-fn args))
    t)
