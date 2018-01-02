;;; feature/workspaces/autoload/workspaces.el -*- lexical-binding: t; -*-

(defvar +workspace-data-file "_workspaces"
  "The file basename in which to store single workspace perspectives.")

(defvar +workspace--last nil)
(defvar +workspace--index 0)

;;
(defface +workspace-tab-selected-face '((t (:inherit 'highlight)))
  "The face for selected tabs displayed by `+workspace/display'"
  :group 'doom)

(defface +workspace-tab-face '((t (:inherit 'default)))
  "The face for selected tabs displayed by `+workspace/display'"
  :group 'doom)


;;
;; Library
;;

(defun +workspace--protected-p (name)
  (equal name persp-nil-name))

(defun +workspace--generate-id ()
  (or (cl-loop for name in (+workspace-list-names)
               when (string-match-p "^#[0-9]+$" name)
               maximize (string-to-number (substring name 1)) into max
               finally return (if max (1+ max)))
      1))


;; --- Predicates -------------------------

;;;###autoload
(defalias #'+workspace-p #'persp-p "Return t if OBJ is a perspective hash table.")

;;;###autoload
(defun +workspace-exists-p (name)
  "Returns t if NAME is the name of an existing workspace."
  (cl-assert (stringp name) t)
  (member name (+workspace-list-names)))

;;;###autoload
(defun +workspace-contains-buffer-p (buffer &optional workspace)
  "Return non-nil if buffer is in workspace (defaults to current workspace)."
  (persp-contain-buffer-p buffer (or workspace (+workspace-current)) nil))


;; --- Getters ----------------------------

;;;###autoload
(defun +workspace-get (name &optional noerror)
  "Returns a workspace (perspective hash table) named NAME."
  (when-let* ((persp (persp-get-by-name name)))
    (cond ((+workspace-p persp) persp)
          ((not noerror) (error "'%s' is an invalid workspace" name)))))

;;;###autoload
(defalias '+workspace-current #'get-current-persp)

;;;###autoload
(defun +workspace-current-name ()
  "Get the name of the current workspace."
  (safe-persp-name (get-current-persp)))

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
  (let ((persp (or persp (+workspace-current))))
    (if (eq persp t)
        (cl-remove-if #'persp--buffer-in-persps (buffer-list))
      (cl-assert (+workspace-p persp) t)
      (cl-loop for buf in (buffer-list)
               if (+workspace-contains-buffer-p buf persp)
               collect buf))))


;; --- Actions ----------------------------

;;;###autoload
(defun +workspace-load (name)
  "Loads a single workspace (named NAME) into the current session. Can only
retrieve perspectives that were explicitly saved with `+workspace-save'.

Returns t if successful, nil otherwise."
  (when (+workspace-exists-p name)
    (error "A workspace named '%s' already exists." name))
  (persp-load-from-file-by-names
   (expand-file-name +workspace-data-file persp-save-dir)
   *persp-hash* (list name))
  (+workspace-exists-p name))

;;;###autoload
(defun +workspace-load-session (&optional name)
  "Replace current session with the entire session named NAME. If NAME is nil,
use `persp-auto-save-fname'."
  (persp-load-state-from-file
   (expand-file-name (or name persp-auto-save-fname) persp-save-dir)))

;;;###autoload
(defun +workspace-save (name)
  "Saves a single workspace (NAME) from the current session. Can be loaded again
with `+workspace-load'. NAME can be the string name of a workspace or its
perspective hash table.

Returns t on success, nil otherwise."
  (unless (+workspace-exists-p name)
    (error "'%s' is an invalid workspace" name))
  (let ((fname (expand-file-name +workspace-data-file persp-save-dir)))
    (persp-save-to-file-by-names fname *persp-hash* (list name))
    (and (member name (persp-list-persp-names-in-file fname))
         t)))

;;;###autoload
(defun +workspace-save-session (&optional name)
  "Save a whole session as NAME. If NAME is nil, use `persp-auto-save-fname'.
Return t on success, nil otherwise."
  (let ((fname (expand-file-name (or name persp-auto-save-fname)
                                 persp-save-dir))
        (persp-auto-save-opt
         (if (or (not name)
                 (equal name persp-auto-save-fname))
             0
           persp-auto-save-opt)))
    (and (persp-save-state-to-file fname) t)))

;;;###autoload
(defun +workspace-new (name)
  "Create a new workspace named NAME. If one already exists, return nil.
Otherwise return t on success, nil otherwise."
  (when (+workspace--protected-p name)
    (error "Can't create a new '%s' workspace" name))
  (when (+workspace-exists-p name)
    (error "A workspace named '%s' already exists" name))
  (and (persp-add-new name) t))

;;;###autoload
(defun +workspace-rename (name new-name)
  "Rename the current workspace named NAME to NEW-NAME. Returns old name on
success, nil otherwise."
  (when (+workspace--protected-p name)
    (error "Can't rename '%s' workspace" name))
  (persp-rename new-name (+workspace-get name)))

;;;###autoload
(defun +workspace-delete (name &optional inhibit-kill-p)
  "Delete the workspace denoted by NAME, which can be the name of a perspective
or its hash table. If INHIBIT-KILL-P is non-nil, don't kill this workspace's
buffers."
  (when (+workspace--protected-p name)
    (error "Can't delete '%s' workspace" name))
  (+workspace-get name) ; error checking
  (persp-kill name inhibit-kill-p)
  (not (+workspace-exists-p name)))

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
      (completing-read
       "Workspace to load: "
       (persp-list-persp-names-in-file
        (expand-file-name +workspace-data-file persp-save-dir))))))
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
  (interactive (list (read-from-minibuffer "New workspace name: ")))
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
      (+workspace-message
       (let ((workspaces (length (+workspace-list-names))))
         (cond ((> workspaces 1)
                (+workspace-delete name)
                (+workspace-switch
                 (if (+workspace-exists-p +workspace--last)
                     +workspace--last
                   (car (+workspace-list-names))))
                (format "Deleted '%s' workspace" name))
               ((= workspaces 1)
                (format "Can't delete the last workspace!"))
               (t
                (+workspace-delete name)
                (+workspace-switch +workspaces-main t)
                (switch-to-buffer (doom-fallback-buffer))
                (format "No workspaces detected! Auto-creating '%s' workspace" +workspaces-main))))
       'success)
    ('error (+workspace-error (cadr ex) t))))

;;;###autoload
(defun +workspace/kill-session ()
  "Delete the current session, clears all workspaces, windows and buffers."
  (interactive)
  (unless (cl-every #'+workspace-delete (+workspace-list-names))
    (+workspace-error "Could not clear session"))
  (+workspace-switch +workspaces-main t)
  (doom/kill-all-buffers)
  (let ((fallback-buf (doom-fallback-buffer)))
    (switch-to-buffer fallback-buf)
    (doom/cleanup-session)))

;;;###autoload
(defun +workspace/kill-session-and-quit ()
  "Forgets current session and quits."
  (interactive)
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
      (cond ((or (+workspace--protected-p current-persp-name)
                 (> (length (doom-visible-windows)) 1))
             (if (bound-and-true-p evil-mode)
                 (evil-window-delete)
               (delete-window)))
            ((> (length (+workspace-list-names)) 1)
             (+workspace/delete current-persp-name))))))

;;;###autoload
(defun +workspace/close-workspace-or-frame ()
  "Close the current workspace. If it's the last, delete the frame instead."
  (interactive)
  (let ((frames (length (frame-list)))
        (workspaces (length (+workspace-list-names))))
    (cond ((> workspaces 1)
           (call-interactively #'+workspace/delete))
          ((> frames 1)
           (call-interactively #'delete-frame))
          (t
           (error "Can't delete last frame.")))))


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
              (propertize (format " [%d] %s " (1+ i) name)
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

;;;###autoload
(defun +workspaces*switch-project-by-name (orig-fn &rest args)
  "Switch to a project and prompt for a file to open.

Ensures the scratch (or dashboard) buffers are CDed into the project's root."
  (when persp-mode
    (+workspace-switch (car args) t)
    (with-current-buffer (switch-to-buffer (doom-fallback-buffer))
      (setq default-directory (car args))))
  (apply orig-fn args))

