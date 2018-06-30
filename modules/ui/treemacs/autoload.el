;;; ui/treemacs/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +treemacs/toggle ()
  "Initialize or toggle treemacs.
* If the treemacs window is visible hide it.
* If a treemacs buffer exists, but is not visible show it.
* If no treemacs buffer exists for the current frame create and show it.
* If the workspace is empty additionally ask for the root path of the first
  project to add."
  (interactive)
  (require 'treemacs)
  (pcase (treemacs--current-visibility)
    (`visible (delete-window (treemacs--is-visible?)))
    (`exists  (treemacs-select-window))
    (`none
     (let ((project-root (doom-project-root 'nocache)))
       (when project-root
         (unless (treemacs--find-project-for-path project-root)
           (treemacs-add-project-at (treemacs--canonical-path project-root)
                                    (doom-project-name 'nocache))))
       (treemacs--init project-root)))))
