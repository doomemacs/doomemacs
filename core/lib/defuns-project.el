;;; defuns-project.el

;;;###autoload
(defun narf/project-root (&optional strict-p)
  "Get the path to the root of your project. Uses `narf-project-root-files' to
determine if a directory is a project."
  (let ((home (file-truename "~")))
    (catch 'found
      (f-traverse-upwards
       (lambda (path)
         (let ((path (file-truename path)))
           (if (file-equal-p home path)
               (throw 'found (if strict-p nil default-directory))
             (dolist (file narf-project-root-files)
               (when (file-exists-p (expand-file-name file path))
                 (throw 'found path)))))) default-directory)
      default-directory)))

;;;###autoload
(defun narf/project-has-files (files &optional root)
  "Return non-nil if `file' exists in the project root."
  (let ((root (or root (narf/project-root)))
        (files (if (listp files) files (list files)))
        found-p file)
    (while (and files (not found-p))
      (setq file (pop files))
      (setq found-p (file-exists-p (narf/project-path-to file root))))
    found-p))

;;;###autoload
(defun narf/project-path-to (file &optional root)
  (let ((root (or root (narf/project-root))))
    (expand-file-name file root)))

;;;###autoload
(defun narf/project-name (&optional root)
  (file-name-nondirectory (directory-file-name (or root (narf/project-root)))))

;;;###autoload
(defun narf/project-p ()
  (not (null (narf/project-root t))))

(provide 'defuns-project)
;;; defuns-project.el ends here
