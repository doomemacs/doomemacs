;;; config/default/autoload/files.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +default/browse-project ()
  "Browse files from the current project's root."
  (interactive) (doom-project-browse (doom-project-root)))
;; NOTE No need for find-in-project, use `projectile-find-file'

;;;###autoload
(defun +default/browse-templates ()
  "Browse files from `+file-templates-dir'."
  (interactive) (doom-project-browse +file-templates-dir))
;;;###autoload
(defun +default/find-in-templates ()
  "Find a file under `+file-templates-dir', recursively."
  (interactive) (doom-project-find-file +file-templates-dir))

;;;###autoload
(defun +default/browse-emacsd ()
  "Browse files from `doom-emacs-dir'."
  (interactive) (doom-project-browse doom-emacs-dir))
;;;###autoload
(defun +default/find-in-emacsd ()
  "Find a file under `doom-emacs-dir', recursively."
  (interactive) (doom-project-find-file doom-emacs-dir))

;;;###autoload
(defun +default/browse-notes ()
  "Browse files from `org-directory'."
  (interactive) (doom-project-browse org-directory))
;;;###autoload
(defun +default/find-in-notes ()
  "Find a file under `org-directory', recursively."
  (interactive) (doom-project-find-file org-directory))

;;;###autoload
(defun +default/find-file-under-here ()
  "Perform a recursive file search from the current directory."
  (interactive)
  (if (featurep! :completion ivy)
      (call-interactively #'counsel-file-jump)
    (λ! (doom-project-find-file default-directory))))

;;;###autoload
(defun +default/discover-projects (arg)
  "Discover projects in `projectile-project-search-path'.
If prefix ARG is non-nil, prompt for the search path."
  (interactive "P")
  (if arg
      (call-interactively #'projectile-discover-projects-in-directory)
    (mapc #'projectile-discover-projects-in-directory projectile-project-search-path)))
