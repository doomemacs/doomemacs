;;; defuns-project.el

;;;###autoload
(defun doom/project-root (&optional strict-p)
  "Get the path to the root of your project. Uses `doom-project-root-files' to
determine if a directory is a project."
  (let (projectile-require-project-root strict-p)
    (projectile-project-root)))

;;;###autoload
(defun doom/project-has-files (files &optional root)
  "Return non-nil if FILES exist in the project root."
  (let ((root (or root (doom/project-root)))
        (files (if (listp files) files (list files)))
        (found-p (if files t)))
    (while (and found-p files)
      (let ((file (expand-file-name (pop files) root)))
        (setq found-p (if (string-suffix-p "/" file)
                          (file-directory-p file)
                        (file-exists-p file)))))
    found-p))

;;;###autoload
(defalias 'doom/project-p 'projectile-project-p)

;;;###autoload
(defalias 'doom/project-name 'projectile-project-name)

(provide 'defuns-project)
;;; defuns-project.el ends here
