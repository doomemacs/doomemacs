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
