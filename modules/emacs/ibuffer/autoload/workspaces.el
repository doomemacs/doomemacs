;;; emacs/ibuffer/autoload/workspaces.el -*- lexical-binding: t; -*-
;;;###if (featurep! :ui workspaces)

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
    (if (not (buffer-live-p buf))
        (user-error "Not a valid or live buffer: %s" buf)
      (when-let (workspaces
                 (cl-loop for wk in (+workspace-list)
                          if (+workspace-contains-buffer-p buf wk)
                          collect wk))
        (+workspace-switch
         (persp-name
          (if (and (not select-first) (cdr workspaces))
              (+workspace-get
               (or (completing-read "Select workspace: " (mapcar #'persp-name workspaces))
                   (user-error "Aborted")))
            (car workspaces)))))
      (persp-add-buffer buf) ; then add the buffer to the current workspace
      (switch-to-buffer buf))))
