;;; core/autoload/projects.el -*- lexical-binding: t; -*-

;;
;; Macros
;;

;;;###autoload
(defmacro without-project-cache! (&rest body)
  "Run BODY with projectile's project-root cache disabled. This is necessary if
you want to interactive with a project other than the one you're in."
  `(let (projectile-project-name
         projectile-require-project-root
         projectile-cached-buffer-file-name
         projectile-cached-project-root)
     ,@body))

;;;###autoload
(defmacro project-file-exists-p! (files)
  "Checks if the project has the specified FILES.
Paths are relative to the project root, unless they start with ./ or ../ (in
which case they're relative to `default-directory'). If they start with a slash,
they are absolute."
  `(file-exists-p! ,files (doom-project-root)))


;;
;; Commands
;;

;;;###autoload
(defun doom//reload-project ()
  "Reload the project root cache."
  (interactive)
  (projectile-invalidate-cache nil)
  (projectile-reset-cached-project-root)
  (dolist (fn projectile-project-root-files-functions)
    (remhash (format "%s-%s" fn default-directory) projectile-project-root-cache)))


;;
;; Library
;;

;;;###autoload
(defun doom-project-p (&optional nocache)
  "Return t if this buffer is currently in a project.
If NOCACHE, don't fetch a cached answer."
  (if nocache
      (without-project-cache! (doom-project-p nil))
    (let ((projectile-require-project-root t))
      (and (projectile-project-p) t))))

;;;###autoload
(defun doom-project-name (&optional nocache)
  "Return the name of the current project.
If NOCACHE, don't fetch a cached answer."
  (if nocache
      (without-project-cache! (doom-project-name nil))
    (projectile-project-name)))

;;;###autoload
(defun doom-project-root (&optional nocache)
  "Returns the root of your project, or `default-directory' if none was found.
If NOCACHE, don't fetch a cached answer."
  (if nocache
      (without-project-cache! (doom-project-root nil))
    (let (projectile-require-project-root)
      (projectile-project-root))))

;;;###autoload
(defalias 'doom-project-expand #'projectile-expand-root)

;;;###autoload
(defun doom-project-find-file (dir)
  "Fuzzy-find a file under DIR."
  (let ((default-directory dir))
    (without-project-cache!
     (call-interactively
      ;; completion modules may remap this command
      (or (command-remapping #'projectile-find-file)
          #'projectile-find-file)))))

;;;###autoload
(defun doom-project-browse (dir)
  "Traverse a file structure starting linearly from DIR."
  (let ((default-directory dir))
    (call-interactively
     ;; completion modules may remap this command
     (or (command-remapping #'find-file)
         #'find-file))))
