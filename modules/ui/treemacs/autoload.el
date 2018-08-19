;;; ui/treemacs/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +treemacs/toggle ()
  "Initialize or toggle treemacs.

* If the treemacs window is visible hide it.
* If a treemacs buffer exists, but is not visible show it.
* If no treemacs buffer exists for the current frame create and show it.
* If the workspace is empty, add the current project to it automatically."
  (interactive)
  (require 'treemacs)
  (let ((origin-buffer (current-buffer)))
    (cl-letf (((symbol-function 'treemacs-workspace->is-empty?)
               (symbol-function 'ignore)))
      (treemacs--init))
    ;;
    (treemacs-do-add-project-to-workspace
     (treemacs--canonical-path (doom-project-root 'nocache))
     (doom-project-name 'nocache))
    ;;
    (setq treemacs--ready-to-follow t)
    (when (or treemacs-follow-after-init treemacs-follow-mode)
      (with-current-buffer origin-buffer
        (treemacs--follow)))))
