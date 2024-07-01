;;; ui/treemacs/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +treemacs/toggle ()
  "Initialize or toggle treemacs.

Ensures that only the current project is present and all other projects have
been removed.

Use `treemacs' command for old functionality."
  (interactive)
  (require 'treemacs)
  (pcase (treemacs-current-visibility)
    (`visible (delete-window (treemacs-get-local-window)))
    (_ (if (treemacs--find-current-user-project)
           (treemacs-add-and-display-current-project-exclusively)
         (treemacs)))))
