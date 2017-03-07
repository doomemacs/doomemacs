;;; feature/workspaces/autoload/workspaces.el

(defvar +workspace-workspace-file "_workspaces"
  "The file basename in which to store single workspace perspectives.")

(defface +workspace-tab-selected-face
  '((((class color) (background light))
     (:background "#333333" :foreground "#000000")) ;; FIXME
    (((class color) (background dark))
     (:background "#51afef" :foreground "#181e26")))
  "The face for selected tabs displayed by `+workspace/display'")

(defface +workspace-tab-face
  '((((class color) (background light))
     (:background "#333333" :foreground "#000000")) ;; FIXME
    (((type graphic) (class color) (background dark))
     (:background "#23272e" :foreground "#5B6268"))
    (((class color) (background dark))
     (:background "#262626" :foreground "#525252")))
  "The face for selected tabs displayed by `+workspace/display'")

;;;###autoload
(defun +workspace-list (&optional exclude-nil-p)
  "Retrieve a list of names of open workspaces (strings)."
  (let ((persps (persp-names)))
    (if exclude-nil-p
        (delete persp-nil-name persps)
      persps)))

;;;###autoload
(defun +workspace-p (obj)
  "Return t if OBJ is a perspective hash table."
  (and obj
       (hash-table-p obj)
       (perspective-p obj)))

;;;###autoload
(defun +workspace-exists-p (name)
  "Returns t if NAME is the name of an existing workspace."
  (when (symbolp name)
    (setq name (symbol-name name)))
  (unless (stringp name)
    (error "Expected a string, got a %s" (type-of name)))
  (member name (+workspace-list)))

;;;###autoload
(defun +workspace-get (name &optional noerror)
  "Returns a workspace (perspective hash table) named NAME."
  (unless (equal name persp-nil-name)
    (let ((persp (persp-get-by-name name)))
      (unless (or persp noerror)
        (error "%s is not an available workspace" name))
      persp)))

;;;###autoload
(defun +workspace-current-name ()
  "Get the name of the currently active workspace."
  (safe-persp-name (get-current-persp)))

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
  (let ((fname (or name persp-auto-save-fname)))
    (and (persp-save-state-to-file fname) t)))

;;;###autoload
(defun +workspace-new (name)
  "Create a new workspace named NAME. If one already exists, return nil.
Otherwise return t on success, nil otherwise."
  (when (equal name persp-nil-name)
    (error "Can't create a new %s workspace" name))
  (unless (+workspace-exists-p name)
    (and (persp-add-new name) t)))

;;;###autoload
(defun +workspace-rename (name new-name)
  "Rename the current workspace named NAME to NEW-NAME. Returns old name on
success, nil otherwise."
  (when (equal name persp-nil-name)
    (error "Can't rename main workspace"))
  (persp-rename new-name (+workspace-get name)))

;;;###autoload
(defun +workspace-delete (name &optional inhibit-kill-p)
  "Delete the workspace denoted by TARGET, which can be the name of a
perspective or its hash table."
  (when (equal name persp-nil-name)
    (error "Can't delete main workspace"))
  (+workspace-get name) ;; error checking
  (persp-kill name inhibit-kill-p))

;;;###autoload
(defun +workspace-switch (name)
  "Switch to another workspace."
  (unless (+workspace-exists-p name)
    (error "%s is not an available workspace" name))
  (persp-frame-switch name))

(defun +workspace--generate-id ()
  (let ((numbers (mapcar (lambda (it) (string-to-number (substring it 1)))
                         (cl-remove-if-not (lambda (it) (string-match-p "^#[0-9]+$" it))
                                           (+workspace-list)))))
    (if numbers
        (1+ (car (sort numbers (lambda (it other) (> it other)))))
      1)))


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
  (let ((name (or name "last")))
    (+workspace-load-session name)
    (+workspace-message (format "'%s' workspace loaded" name) 'success)))

;;;###autoload
(defun +workspace/save (name)
  "Save the current workspace. If called with C-u, autosave the current
workspace."
  (interactive
   (list
    (if current-prefix-arg
        (+workspace-current-name)
      (completing-read "Workspace to save: " (+workspace-list)))))
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
      (let ((name (or name "last")))
        (if (+workspace-save-session name)
            (+workspace-message (format "Saved session as %s" name) 'success)
          (error "Couldn't save session as %s" name)))
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
                           (+workspace-list)
                           nil nil current-name)
        current-name))))
  (condition-case ex
      (if (+workspace-delete name)
          (+workspace-message (format "Deleted %s workspace" name) 'success)
        (error "Couldn't delete %s workspace" name))
    ('error (+workspace-error (cadr ex) t))))

;;;###autoload
(defun +workspace/kill-session ()
  "Delete the current session, clears all workspaces, windows and buffers."
  (interactive)
  (unless (cl-every '+workspace-delete (+workspace-list t))
    (+workspace-error "Could not clear session"))
  (+workspace-switch persp-nil-name)
  (doom/kill-all-buffers)
  (let ((fallback-buf (doom-fallback-buffer)))
    (switch-to-buffer fallback-buf)
    (dolist (buf (buffer-list))
      (unless (eq (buffer-name buf) fallback-buf)
        (persp-remove-buffer buf)
        (kill-buffer buf)))))

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
          (persp-frame-switch name)
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
             (completing-read "Switch to workspace: " (+workspace-list)))))
  (when (and (stringp index)
             (string-match-p "^[0-9]+$" index))
    (setq index (string-to-number index)))
  (condition-case ex
      (let ((names (+workspace-list))
            (old-name (+workspace-current-name)))
        (cond ((numberp index)
               (let ((dest (nth index names)))
                 (unless dest
                   (error "No workspace at #%s" (1+ index)))
                 (persp-switch dest)))
              ((stringp index)
               (unless (member index names)
                 (error "No workspace named %s" index))
               (persp-frame-switch index)))
        (unless (called-interactively-p 'interactive)
          (if (equal (+workspace-current-name) old-name)
              (+workspace-message (format "Already in %s" old-name) 'warn)
            (+workspace/display))))
    ('error (+workspace-error (cadr ex) t))))

;;;###autoload
(defun +workspace/switch-to-last ()
  "Switch to the last workspace."
  (interactive)
  (+workspace/switch-to (car (last (+workspace-list)))))

;;;###autoload
(defun +workspace/cycle (n)
  "Cycle n workspaces to the right (default) or left."
  (interactive (list 1))
  (condition-case ex
      (let ((persp-switch-wrap t))
        (dotimes (i n)
          (if (> n 0)
              (persp-next)
            (persp-prev)))
        (unless (called-interactively-p 'interactive)
          (+workpace/display)))
    ('error (+workspace-error (cadr ex) t))))

;;;###autoload
(defun +workspace/close-window-or-workspace ()
  "Close the selected window. If it's the last window in the workspace, close
the workspace and move to the next."
  (interactive)
  (if (doom-popup-p)
      (doom/popup-close)
    (let ((current-persp-name (+workspace-current-name)))
      (cond ((or (equal current-persp-name persp-nil-name)
                 (> (length (doom-visible-windows)) 1))
             (if (bound-and-true-p evil-mode)
                 (evil-window-delete)
               (delete-window)))
            ((> (length (+workspace-list)) 1)
             (+workspace/delete current-persp-name))))))


;;
;; Tabs display in minibuffer
;;

(defun +workspace--tabline (&optional names)
  (let ((names (or names (+workspace-list)))
        (current-name (+workspace-current-name))
        (i 0))
    (mapconcat
     'identity
     (mapcar (lambda (it)
               (cl-incf i)
               (propertize (format " [%d] %s " i it)
                           'face (if (equal current-name it)
                                     '+workspace-tab-selected-face
                                   '+workspace-tab-face)))
             names)
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
  (funcall (if noerror 'message 'error) "%s" (+workspace--message-body message 'error)))

;;;###autoload
(defun +workspace/display ()
  "Display a list of workspaces (like tabs) in the echo area."
  (interactive)
  (message "%s" (+workspace--tabline)))

