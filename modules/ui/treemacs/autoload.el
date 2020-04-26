;;; ui/treemacs/autoload.el -*- lexical-binding: t; -*-

(defun +treemacs--init ()
  (require 'treemacs)
  (let ((origin-buffer (current-buffer)))
    ;; Toggle treemacs without prompting for the first project.
    (cl-letf (((symbol-function 'treemacs-workspace->is-empty?)
               (symbol-function 'ignore)))
      (treemacs--init))
    (unless (bound-and-true-p persp-mode)
      (dolist (project (treemacs-workspace->projects (treemacs-current-workspace)))
        (treemacs-do-remove-project-from-workspace project)))
    (with-current-buffer origin-buffer
      (let ((project-root (or (doom-project-root) default-directory)))
        (treemacs-do-add-project-to-workspace
         (treemacs--canonical-path project-root)
         (doom-project-name project-root)))
      (setq treemacs--ready-to-follow t)
      (when (or treemacs-follow-after-init treemacs-follow-mode)
        (treemacs--follow)))))

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
    (_ (+treemacs--init))))

;;;###autoload
(defun +treemacs/find-file (arg)
  "Open treemacs (if necessary) and find current file."
  (interactive "P")
  (let ((origin-buffer (current-buffer)))
    (+treemacs--init)
    (with-current-buffer origin-buffer
      (treemacs-find-file arg))))
