;;; defuns-project.el

;;;###autoload
(defun narf/project-root (&optional strict-p)
  "Get the path to the root of your project. Uses `narf-project-root-files' to
determine if a directory is a project."
  (let (projectile-require-project-root strict-p)
    (projectile-project-root)))

;;;###autoload
(defun narf/project-has-files (files &optional root)
  "Return non-nil if FILES exist in the project root."
  (let ((root (or root (narf/project-root)))
        (files (if (listp files) files (list files)))
        (found-p (if files t)))
    (while (and found-p files)
      (let ((file (expand-file-name (pop files) root)))
        (setq found-p (if (string-suffix-p "/" file)
                          (file-directory-p file)
                        (file-exists-p file)))))
    found-p))

;;;###autoload
(defalias 'narf/project-p 'projectile-project-p)

(provide 'defuns-project)
;;; defuns-project.el ends here
