;;; emacs/ibuffer/autoload/workspaces.el -*- lexical-binding: t; -*-
;;;###if (modulep! :ui workspaces)

;;;###autoload
(defun +ibuffer-workspace (workspace-name)
  "Open an ibuffer window for a workspace"
  (ibuffer nil (format "%s buffers" workspace-name)
           (list (cons 'workspace-buffers (+workspace-get workspace-name)))))

;;;###autoload
(defun +ibuffer/open-for-current-workspace ()
  "Open an ibuffer window for the current workspace"
  (interactive)
  (+ibuffer-workspace (+workspace-current-name)))

;;;###autoload
(defun +ibuffer/visit-workspace-buffer (&optional select-first)
  "Visit buffer, but switch to its workspace if it exists."
  (interactive "P")
  (let ((buf (ibuffer-current-buffer t)))
    (unless (buffer-live-p buf)
      (user-error "Not a valid or live buffer: %s" buf))
    (if-let* ((workspaces
               (cl-loop for wk in (+workspace-list)
                        if (+workspace-contains-buffer-p buf wk)
                        collect wk)))
        (+workspace-switch
         (if (and (not select-first) (cdr workspaces))
             (or (completing-read "Select workspace: " (mapcar #'persp-name workspaces))
                 (user-error "Aborted"))
           (persp-name (car workspaces))))
      ;; Or add the buffer to the current workspace
      (persp-add-buffer buf))
    (switch-to-buffer buf)))
