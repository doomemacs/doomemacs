;;; core/autoload/projects.el -*- lexical-binding: t; -*-

;;
;; Macros

;;;###autoload
(defmacro without-project-cache! (&rest body)
  "Run BODY with projectile's project-root cache disabled. This is necessary if
you want to interactive with a project other than the one you're in."
  `(let ((projectile-project-root-cache (make-hash-table :test 'equal))
         projectile-project-name
         projectile-require-project-root)
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

;;;###autoload
(defun doom/reload-project ()
  "Reload the project root cache."
  (interactive)
  (projectile-invalidate-cache nil)
  (setq-default projectile-project-root nil)
  (dolist (fn projectile-project-root-files-functions)
    (remhash (format "%s-%s" fn default-directory) projectile-project-root-cache)))


;;
;; Library

;;;###autoload
(defalias 'doom-project-p #'projectile-project-p)

;;;###autoload
(defalias 'doom-project-root #'projectile-project-root)

;;;###autoload
(defun doom-project-name (&optional dir)
  "Return the name of the current project."
  (let ((project-root (or (projectile-project-root dir)
                          (if dir (expand-file-name dir)))))
    (if project-root
        (funcall projectile-project-name-function project-root)
      "-")))

;;;###autoload
(defun doom-project-expand (name &optional dir)
  "Expand NAME to project root."
  (expand-file-name name (projectile-project-root dir)))

;;;###autoload
(defun doom-project-find-file (dir)
  "Fuzzy-find a file under DIR."
  (without-project-cache!
   (let* ((default-directory (file-truename dir))
          projectile-project-root)
     (call-interactively
      ;; completion modules may remap this command
      (or (command-remapping #'projectile-find-file)
          #'projectile-find-file)))))

;;;###autoload
(defun doom-project-browse (dir)
  "Traverse a file structure starting linearly from DIR."
  (let ((default-directory (file-truename dir)))
    (call-interactively
     ;; completion modules may remap this command
     (or (command-remapping #'find-file)
         #'find-file))))
