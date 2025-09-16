;;; ui/workspaces/autoload/workspaces.el -*- lexical-binding: t; -*-


;;
;;; Public

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
   (if (null tabs)
       (append (frame-parameter nil 'buffer-list)
               (frame-parameter nil 'buried-buffer-list))
     (cl-delete-duplicates
      (cl-loop for tab in tabs
               if (and tab (not (eq (car-safe tab) 'current-tab)))
               nconc (or (cdr (assq 'wc-bl tab))
                         (mapcar #'get-buffer
                                 (car (cdr (assq #'tabspaces--buffer-list
                                                 (assq 'ws tab)))))))
      :test #'eq))))

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
(defun +workspaces-get (name &optional frame)
  "TODO"
  (when-let* ((idx (tab-bar--tab-index-by-name name nil frame))
              (tab (nth idx (funcall tab-bar-tabs-function frame))))
    (if (eq (car tab) 'current-tab)
        (tab-bar--tab frame)
      tab)))

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
                            (tab-bar--tabs-recent))))
    (unless (member name workspaces)
      (when required?
        (user-error "No workspace: %s" name)))
    (tab-bar-select-tab-by-name name)))


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
(defun +workspaces/kill ()
  "Kill all buffers in the workspace and then close the workspace itself."
  (interactive)
  (let ((tab-buffers (+workspaces-buffer-list))
        (other-buffers (+workspaces-other-buffer-list)))
    (unwind-protect
        (cl-loop for b in tab-buffers
                 unless (member b other-buffers)  ; only kill if not open elsewhere
                 when (buffer-live-p b)
                 do (kill-buffer b))
      (tab-bar-close-tab))))

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

;;; workspaces.el ends here
