;;; emacs/ibuffer/autoload/workspaces.el -*- lexical-binding: t; -*-
;;;###if (modulep! :ui workspaces)

;;;###autoload
(defun +ibuffer-workspace (workspace-name)
  "Open an ibuffer window for a workspace"
  (ibuffer nil (format "%s buffers" workspace-name)
           (list (cons 'workspace-buffers
                       (+workspaces-buffer-list (+workspaces-get workspace-name))))))

;;;###autoload
(defun +ibuffer/open-for-current-workspace ()
  "Open an ibuffer window for the current workspace"
  (interactive)
  (+ibuffer-workspace (+workspaces-current-name)))

;;;###autoload
(defun +ibuffer/visit-workspace-buffer (buffer &optional select-first?)
  "Visit buffer, but switch to its workspace if it exists."
  (interactive
   (list (ibuffer-current-buffer t) current-prefix-arg))
  (if-let* ((workspaces
             (cl-loop for wk in (+workspaces-list)
                      if (+workspaces-contain-buffer-p buffer wk)
                      collect wk)))
      (let ((target-name
             (if (and (not select-first?) (cdr workspaces))
                 (or (completing-read
                      "Select workspace: " (mapcar (lambda (ws) (alist-get 'name ws))
                                                   workspaces))
                     (user-error "Aborted"))
               (alist-get 'name (car workspaces)))))
        (if (equal (+workspaces-current-name) target-name)
            (switch-to-buffer buffer)
          (+workspaces-switch target-name t)))
    ;; Or add the buffer to the current workspace
    (switch-to-buffer buffer)))
