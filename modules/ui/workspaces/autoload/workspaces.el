;;; ui/workspaces/autoload/workspaces.el -*- lexical-binding: t; -*-

(defvar +workspace--last nil)
(defvar +workspace--index 0)

;;;###autoload
(defface +workspace-tab-selected-face '((t (:inherit highlight)))
  "The face for selected tabs displayed by `+workspace/display'"
  :group 'persp-mode)

;;;###autoload
(defface +workspace-tab-face '((t (:inherit default)))
  "The face for selected tabs displayed by `+workspace/display'"
  :group 'persp-mode)


;;
;;; Library

(defun +workspace--protected-p (name)
  (equal name persp-nil-name))

(defun +workspace--generate-id ()
  (or (cl-loop for name in (+workspace-list-names)
               when (string-match-p "^#[0-9]+$" name)
               maximize (string-to-number (substring name 1)) into max
               finally return (if max (1+ max)))
      1))


;;; Predicates
;;;###autoload
(defalias #'+workspace-p #'perspective-p
  "Return t if OBJ is a perspective hash table.")

;;;###autoload
(defun +workspace-exists-p (name)
  "Returns t if NAME is the name of an existing workspace."
  (member name (+workspace-list-names)))

;;;###autoload
(defalias #'+workspace-contains-buffer-p #'persp-contain-buffer-p
  "Return non-nil if BUFFER is in WORKSPACE (defaults to current workspace).")


;;; Getters
;;;###autoload
(defalias #'+workspace-current #'get-current-persp
  "Return the currently active workspace.")

;;;###autoload
(defun +workspace-get (name &optional noerror)
  "Return a workspace named NAME. Unless NOERROR is non-nil, this throws an
error if NAME doesn't exist."
  (cl-check-type name string)
  (when-let (persp (persp-get-by-name name))
    (cond ((+workspace-p persp) persp)
          ((not noerror)
           (error "No workspace called '%s' was found" name)))))

;;;###autoload
(defun +workspace-current-name ()
  "Get the name of the current workspace."
  (safe-persp-name (+workspace-current)))

;;;###autoload
(defun +workspace-list ()
  "Return a list of workspace structs (satisifes `+workspace-p')."
  ;; We don't use `hash-table-values' because it doesn't ensure order in older
  ;; versions of Emacs
  (cl-loop for name in persp-names-cache
           if (gethash name *persp-hash*)
           collect it))

;;;###autoload
(defun +workspace-list-names ()
  "Return the list of names of open workspaces."
  persp-names-cache)

;;;###autoload
(defun +workspace-buffer-list (&optional persp)
  "Return a list of buffers in PERSP.

PERSP can be a string (name of a workspace) or a workspace (satisfies
`+workspace-p'). If nil or omitted, it defaults to the current workspace."
  (let ((persp (or persp (+workspace-current))))
    (unless (+workspace-p persp)
      (user-error "Not in a valid workspace (%s)" persp))
    (persp-buffers persp)))

;;;###autoload
(defun +workspace-orphaned-buffer-list ()
  "Return a list of buffers that aren't associated with any perspective."
  (cl-remove-if #'persp--buffer-in-persps (buffer-list)))


;;; Actions
;;;###autoload
(defun +workspace-load (name)
  "Loads a single workspace (named NAME) into the current session. Can only
retrieve perspectives that were explicitly saved with `+workspace-save'.

Returns t if successful, nil otherwise."
  (when (+workspace-exists-p name)
    (user-error "A workspace named '%s' already exists." name))
  (persp-load-from-file-by-names
   (expand-file-name +workspaces-data-file persp-save-dir)
   *persp-hash* (list name))
  (+workspace-exists-p name))

;;;###autoload
(defun +workspace-save (name)
  "Saves a single workspace (NAME) from the current session. Can be loaded again
with `+workspace-load'. NAME can be the string name of a workspace or its
perspective hash table.

Returns t on success, nil otherwise."
  (unless (+workspace-exists-p name)
    (error "'%s' is an invalid workspace" name))
  (let ((fname (expand-file-name +workspaces-data-file persp-save-dir)))
    (persp-save-to-file-by-names fname *persp-hash* (list name))
    (and (member name (persp-list-persp-names-in-file fname))
         t)))

;;;###autoload
(defun +workspace-new (name)
  "Create a new workspace named NAME. If one already exists, return nil.
Otherwise return t on success, nil otherwise."
  (when (+workspace--protected-p name)
    (error "Can't create a new '%s' workspace" name))
  (when (+workspace-exists-p name)
    (error "A workspace named '%s' already exists" name))
  (let ((persp (persp-add-new name))
        (+popup--inhibit-transient t))
    (save-window-excursion
      (let ((ignore-window-parameters t)
            (+popup--inhibit-transient t))
        (persp-delete-other-windows))
      (switch-to-buffer (doom-fallback-buffer))
      (setf (persp-window-conf persp)
            (funcall persp-window-state-get-function (selected-frame))))
    persp))

;;;###autoload
(defun +workspace-rename (name new-name)
  "Rename the current workspace named NAME to NEW-NAME. Returns old name on
success, nil otherwise."
  (when (+workspace--protected-p name)
    (error "Can't rename '%s' workspace" name))
  (persp-rename new-name (+workspace-get name)))

;;;###autoload
(defun +workspace-delete (workspace &optional inhibit-kill-p)
  "Delete the workspace denoted by WORKSPACE, which can be the name of a perspective
or its hash table. If INHIBIT-KILL-P is non-nil, don't kill this workspace's
buffers."
  (unless (stringp workspace)
    (setq workspace (persp-name workspace)))
  (when (+workspace--protected-p workspace)
    (error "Can't delete '%s' workspace" workspace))
  (+workspace-get workspace) ; error checking
  (persp-kill workspace inhibit-kill-p)
  (not (+workspace-exists-p workspace)))

;;;###autoload
(defun +workspace-switch (name &optional auto-create-p)
  "Switch to another workspace named NAME (a string).

If AUTO-CREATE-P is non-nil, create the workspace if it doesn't exist, otherwise
throws an error."
  (unless (+workspace-exists-p name)
    (if auto-create-p
        (+workspace-new name)
      (error "%s is not an available workspace" name)))
  (let ((old-name (+workspace-current-name)))
    (unless (equal old-name name)
      (setq +workspace--last
            (or (and (not (string= old-name persp-nil-name))
                     old-name)
                +workspaces-main))
      (persp-frame-switch name))
    (equal (+workspace-current-name) name)))


;;
;;; Commands

;;;###autoload
(defalias '+workspace/restore-last-session #'doom/quickload-session)

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
        (expand-file-name +workspaces-data-file persp-save-dir))))))
  (if (not (+workspace-load name))
      (+workspace-error (format "Couldn't load workspace %s" name))
    (+workspace/switch-to name)
    (+workspace/display)))

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
(defun +workspace/rename (new-name)
  "Rename the current workspace."
  (interactive (list (read-from-minibuffer "New workspace name: ")))
  (condition-case-unless-debug ex
      (let* ((current-name (+workspace-current-name))
             (old-name (+workspace-rename current-name new-name)))
        (unless old-name
          (error "Failed to rename %s" current-name))
        (+workspace-message (format "Renamed '%s'->'%s'" old-name new-name) 'success))
    ('error (+workspace-error ex t))))

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
                           nil nil nil nil current-name)
        current-name))))
  (condition-case-unless-debug ex
      ;; REVIEW refactor me
      (let ((workspaces (+workspace-list-names)))
        (if (not (member name workspaces))
            (+workspace-message (format "'%s' workspace doesn't exist" name) 'warn)
          (cond ((delq (selected-frame) (persp-frames-with-persp (get-frame-persp)))
                 (user-error "Can't close workspace, it's visible in another frame"))
                ((not (equal (+workspace-current-name) name))
                 (+workspace-delete name))
                ((cdr workspaces)
                 (+workspace-delete name)
                 (+workspace-switch
                  (if (+workspace-exists-p +workspace--last)
                      +workspace--last
                    (car (+workspace-list-names))))
                 (unless (doom-buffer-frame-predicate (window-buffer))
                   (switch-to-buffer (doom-fallback-buffer))))
                (t
                 (+workspace-switch +workspaces-main t)
                 (unless (string= (car workspaces) +workspaces-main)
                   (+workspace-delete name))
                 (doom/kill-all-buffers (doom-buffer-list))))
          (+workspace-message (format "Deleted '%s' workspace" name) 'success)))
    ('error (+workspace-error ex t))))

;;;###autoload
(defun +workspace/kill-session (&optional interactive)
  "Delete the current session, all workspaces, windows and their buffers."
  (interactive (list t))
  (let ((windows (length (window-list)))
        (persps (length (+workspace-list-names)))
        (buffers 0))
    (let ((persp-autokill-buffer-on-remove t))
      (unless (cl-every #'+workspace-delete (+workspace-list-names))
        (+workspace-error "Could not clear session")))
    (+workspace-switch +workspaces-main t)
    (setq buffers (doom/kill-all-buffers (buffer-list)))
    (when interactive
      (message "Killed %d workspace(s), %d window(s) & %d buffer(s)"
               persps windows buffers))))

;;;###autoload
(defun +workspace/kill-session-and-quit ()
  "Kill emacs without saving anything."
  (interactive)
  (let ((persp-auto-save-opt 0))
    (kill-emacs)))

;;;###autoload
(defun +workspace/new (&optional name clone-p)
  "Create a new workspace named NAME. If CLONE-P is non-nil, clone the current
workspace, otherwise the new workspace is blank."
  (interactive (list nil current-prefix-arg))
  (unless name
    (setq name (format "#%s" (+workspace--generate-id))))
  (condition-case e
      (cond ((+workspace-exists-p name)
             (error "%s already exists" name))
            (clone-p (persp-copy name t))
            (t
             (+workspace-switch name t)
             (+workspace/display)))
    ((debug error) (+workspace-error (cadr e) t))))

;;;###autoload
(defun +workspace/switch-to (index)
  "Switch to a workspace at a given INDEX. A negative number will start from the
end of the workspace list."
  (interactive
   (list (or current-prefix-arg
             (if (featurep! :completion ivy)
                 (ivy-read "Switch to workspace: "
                           (+workspace-list-names)
                           :caller #'+workspace/switch-to
                           :preselect (+workspace-current-name))
               (completing-read "Switch to workspace: " (+workspace-list-names))))))
  (when (and (stringp index)
             (string-match-p "^[0-9]+$" index))
    (setq index (string-to-number index)))
  (condition-case-unless-debug ex
      (let ((names (+workspace-list-names))
            (old-name (+workspace-current-name)))
        (cond ((numberp index)
               (let ((dest (nth index names)))
                 (unless dest
                   (error "No workspace at #%s" (1+ index)))
                 (+workspace-switch dest)))
              ((stringp index)
               (+workspace-switch index t))
              (t
               (error "Not a valid index: %s" index)))
        (unless (called-interactively-p 'interactive)
          (if (equal (+workspace-current-name) old-name)
              (+workspace-message (format "Already in %s" old-name) 'warn)
            (+workspace/display))))
    ('error (+workspace-error (cadr ex) t))))

;;;###autoload
(dotimes (i 9)
  (defalias (intern (format "+workspace/switch-to-%d" i))
    (lambda () (interactive) (+workspace/switch-to i))
    (format "Switch to workspace #%d" (1+ i))))

;;;###autoload
(defun +workspace/switch-to-final ()
  "Switch to the final workspace in open workspaces."
  (interactive)
  (+workspace/switch-to (car (last (+workspace-list-names)))))

;;;###autoload
(defun +workspace/other ()
  "Switch to the last activated workspace."
  (interactive)
  (+workspace/switch-to +workspace--last))

;;;###autoload
(defun +workspace/cycle (n)
  "Cycle n workspaces to the right (default) or left."
  (interactive (list 1))
  (let ((current-name (+workspace-current-name)))
    (if (equal current-name persp-nil-name)
        (+workspace-switch +workspaces-main t)
      (condition-case-unless-debug ex
          (let* ((persps (+workspace-list-names))
                 (perspc (length persps))
                 (index (cl-position current-name persps)))
            (when (= perspc 1)
              (user-error "No other workspaces"))
            (+workspace/switch-to (% (+ index n perspc) perspc))
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
  "Close the selected window. If it's the last window in the workspace, either
close the workspace (as well as its associated frame, if one exists) and move to
the next."
  (interactive)
  (let ((delete-window-fn (if (featurep 'evil) #'evil-window-delete #'delete-window)))
    (if (window-dedicated-p)
        (funcall delete-window-fn)
      (let ((current-persp-name (+workspace-current-name)))
        (cond ((or (+workspace--protected-p current-persp-name)
                   (cdr (doom-visible-windows)))
               (funcall delete-window-fn))

              ((cdr (+workspace-list-names))
               (let ((frame-persp (frame-parameter nil 'workspace)))
                 (if (string= frame-persp (+workspace-current-name))
                     (delete-frame)
                   (+workspace/delete current-persp-name))))

              ((+workspace-error "Can't delete last workspace" t)))))))

;;;###autoload
(defun +workspace/swap-left (&optional count)
  "Swap the current workspace with the COUNTth workspace on its left."
  (interactive "p")
  (let* ((current-name (+workspace-current-name))
         (count (or count 1))
         (index (- (cl-position current-name persp-names-cache :test #'equal)
                   count))
         (names (remove current-name persp-names-cache)))
    (unless names
      (user-error "Only one workspace"))
    (let ((index (min (max 0 index) (length names))))
      (setq persp-names-cache
            (append (cl-subseq names 0 index)
                    (list current-name)
                    (cl-subseq names index))))
    (when (called-interactively-p 'any)
      (+workspace/display))))

;;;###autoload
(defun +workspace/swap-right (&optional count)
  "Swap the current workspace with the COUNTth workspace on its right."
  (interactive "p")
  (funcall-interactively #'+workspace/swap-left (- count)))


;;
;;; Tabs display in minibuffer

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

(defun +workspace--message-body (message &optional type)
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
  (funcall (if noerror #'message #'error)
           "%s" (+workspace--message-body message 'error)))

;;;###autoload
(defun +workspace/display ()
  "Display a list of workspaces (like tabs) in the echo area."
  (interactive)
  (let (message-log-max)
    (message "%s" (+workspace--tabline))))


;;
;;; Hooks

;;;###autoload
(defun +workspaces-delete-associated-workspace-h (&optional frame)
  "Delete workspace associated with current frame.
A workspace gets associated with a frame when a new frame is interactively
created."
  (when persp-mode
    (unless frame
      (setq frame (selected-frame)))
    (let ((frame-persp (frame-parameter frame 'workspace)))
      (when (string= frame-persp (+workspace-current-name))
        (+workspace/delete frame-persp)))))

;;;###autoload
(defun +workspaces-associate-frame-fn (frame &optional _new-frame-p)
  "Create a blank, new perspective and associate it with FRAME."
  (when persp-mode
    (if (not (persp-frame-list-without-daemon))
        (+workspace-switch +workspaces-main t)
      (with-selected-frame frame
        (+workspace-switch (format "#%s" (+workspace--generate-id)) t)
        (unless (doom-real-buffer-p (current-buffer))
          (switch-to-buffer (doom-fallback-buffer)))
        (set-frame-parameter frame 'workspace (+workspace-current-name))
        ;; ensure every buffer has a buffer-predicate
        (persp-set-frame-buffer-predicate frame))
      (run-at-time 0.1 nil #'+workspace/display))))

(defvar +workspaces--project-dir nil)
;;;###autoload
(defun +workspaces-set-project-action-fn ()
  "A `projectile-switch-project-action' that sets the project directory for
`+workspaces-switch-to-project-h'."
  (setq +workspaces--project-dir default-directory))

;;;###autoload
(defun +workspaces-switch-to-project-h (&optional dir)
  "Creates a workspace dedicated to a new project. If one already exists, switch
to it. If in the main workspace and it's empty, recycle that workspace, without
renaming it.

Afterwords, runs `+workspaces-switch-project-function'. By default, this prompts
the user to open a file in the new project.

This be hooked to `projectile-after-switch-project-hook'."
  (when dir
    (setq +workspaces--project-dir dir))
  ;; HACK Clear projectile-project-root, otherwise cached roots may interfere
  ;;      with project switch (see #3166)
  (let (projectile-project-root)
    (when (and persp-mode +workspaces--project-dir)
      (when projectile-before-switch-project-hook
        (with-temp-buffer
          ;; Load the project dir-local variables into the switch buffer, so the
          ;; action can make use of them
          (setq default-directory +workspaces--project-dir)
          (hack-dir-local-variables-non-file-buffer)
          (run-hooks 'projectile-before-switch-project-hook)))
      (unwind-protect
          (if (and (not (null +workspaces-on-switch-project-behavior))
                   (or (eq +workspaces-on-switch-project-behavior t)
                       (equal (safe-persp-name (get-current-persp)) persp-nil-name)
                       (+workspace-buffer-list)))
              (let* ((persp
                      (let ((project-name (doom-project-name +workspaces--project-dir)))
                        (or (+workspace-get project-name t)
                            (+workspace-new project-name))))
                     (new-name (persp-name persp)))
                (+workspace-switch new-name)
                (with-current-buffer (doom-fallback-buffer)
                  (setq default-directory +workspaces--project-dir)
                  (hack-dir-local-variables-non-file-buffer))
                (unless current-prefix-arg
                  (funcall +workspaces-switch-project-function +workspaces--project-dir))
                (+workspace-message
                 (format "Switched to '%s' in new workspace" new-name)
                 'success))
            (with-current-buffer (doom-fallback-buffer)
              (setq default-directory +workspaces--project-dir)
              (hack-dir-local-variables-non-file-buffer)
              (message "Switched to '%s'" (doom-project-name +workspaces--project-dir)))
            (with-demoted-errors "Workspace error: %s"
              (+workspace-rename (+workspace-current-name) (doom-project-name +workspaces--project-dir)))
            (unless current-prefix-arg
              (funcall +workspaces-switch-project-function +workspaces--project-dir)))
        (run-hooks 'projectile-after-switch-project-hook)
        (setq +workspaces--project-dir nil)))))


;;
;;; Advice

;;;###autoload
(defun +workspaces-autosave-real-buffers-a (orig-fn &rest args)
  "Don't autosave if no real buffers are open."
  (when (doom-real-buffer-list)
    (apply orig-fn args))
  t)
