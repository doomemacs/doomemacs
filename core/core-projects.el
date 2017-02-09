;;; core-projects.el --- tools for getting around your project

;; I want Emacs to be nominally aware of the projects. `projectile' provides
;; tools for digging through project files and exposing an API I can use to make
;; other plugins/features project-aware.

(@package projectile :demand t
  :init
  (setq projectile-cache-file (concat doom-cache-dir "/projectile.cache")
        projectile-completion-system 'ivy
        projectile-enable-caching (not noninteractive)
        projectile-file-exists-remote-cache-expire nil
        projectile-globally-ignored-directories `(,doom-cache-dir ".sync")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-globally-ignored-files '(".DS_Store" "Icon")
        projectile-indexing-method 'alien
        projectile-known-projects-file (concat doom-cache-dir "/projectile.projects")
        projectile-project-root-files '(".git" ".hg" ".svn" ".project")
        projectile-require-project-root nil)

  :config
  (projectile-mode +1)

  (mapc (lambda (r) (push r projectile-other-file-alist))
        '(("less" "css")
          ("styl" "css")
          ("sass" "css")
          ("scss" "css")
          ("css" "scss" "sass" "less" "styl")
          ("jade" "html")
          ("pug" "html")
          ("html" "jade" "pug" "jsx" "tsx")))

  (defun doom*projectile-cache-current-file (orig-fun &rest args)
    "Don't cache ignored files."
    (unless (--any (f-descendant-of? buffer-file-name it)
                   (projectile-ignored-directories))
      (apply orig-fun args)))
  (advice-add 'projectile-cache-current-file :around 'doom*projectile-cache-current-file))


;;
;; Library
;;

(defun doom-project-p (&optional strict-p)
  "Whether or not this buffer is currently in a project or not."
  (let ((projectile-require-project-root strict-p))
    (projectile-project-p)))

(defun doom-project-root (&optional strict-p)
  "Get the path to the root of your project."
  (let ((projectile-require-project-root strict-p))
    (ignore-errors (projectile-project-root))))

(defun doom-project-has-files (files &optional root)
  "Return non-nil if FILES exist in the project root."
  (let ((root (or root (doom-project-root)))
        (files (if (listp files) files (list files)))
        (found-p (if files t)))
    (while (and found-p files)
      (let ((file (expand-file-name (pop files) root)))
        (setq found-p (if (string-suffix-p "/" file)
                          (file-directory-p file)
                        (file-exists-p file)))))
    found-p))

(provide 'core-projects)
;;; core-projects.el ends here
