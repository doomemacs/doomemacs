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

;;
;; Navigation
;;

;;;###autoload
(defun doom/switch-to-project-buffer ()
  "Displays open buffers in current project. If ALL-P, then show all open
buffers."
  (interactive)
  (ivy-read "Switch to: " (doom/get-buffer-names t)
            :matcher #'ivy--switch-buffer-matcher
            :preselect (buffer-name (other-buffer (current-buffer)))
            :action #'ivy--switch-buffer-action
            :keymap ivy-switch-buffer-map
            :caller 'doom/switch-to-project-buffer))

;;;###autoload
(defun doom/find-file-in-emacsd ()
  (interactive)
  (let ((default-directory doom-emacs-dir))
    (projectile-find-file)))

;;;###autoload
(defun doom/find-file-in-dotfiles ()
  (interactive)
  (let ((default-directory (expand-file-name ".dotfiles" "~")))
    (projectile-find-file)))

(provide 'defuns-project)
;;; defuns-project.el ends here
