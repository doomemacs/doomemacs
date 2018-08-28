;;; ui/treemacs/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +treemacs/toggle ()
  "Initialize or toggle treemacs.

Ensures that only the current project is present and all other projects have
been removed."
  (interactive)
  (require 'treemacs)
  (let ((origin-buffer (current-buffer)))
    (cl-letf (((symbol-function 'treemacs-workspace->is-empty?)
               (symbol-function 'ignore)))
      (treemacs--init))
    ;;
    (dolist (project (treemacs-workspace->projects (treemacs-current-workspace)))
      (treemacs-do-remove-project-from-workspace project))
    ;;
    (with-current-buffer origin-buffer
      (treemacs-do-add-project-to-workspace
       (treemacs--canonical-path (doom-project-root 'nocache))
       (doom-project-name 'nocache)))
    ;;
    (setq treemacs--ready-to-follow t)
    (when (or treemacs-follow-after-init treemacs-follow-mode)
      (with-current-buffer origin-buffer
        (treemacs--follow)))))
