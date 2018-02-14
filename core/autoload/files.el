;;; core/autoload/files.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom/sudo-find-file (file)
  "Open FILE as root."
  (interactive
   (list (read-file-name "Open as root: ")))
  (find-file (if (file-writable-p file)
                 file
               (concat "/sudo:root@localhost:" file))))

;;;###autoload
(defun doom/sudo-this-file ()
  "Open the current file as root."
  (interactive)
  (doom/sudo-find-file (file-truename buffer-file-name)))


;;
(defun doom--forget-file (old-path &optional new-path)
  "Ensure `recentf', `projectile' and `save-place' forget OLD-PATH."
  (when (bound-and-true-p recentf-mode)
    (when new-path
      (recentf-add-file new-path))
    (recentf-remove-if-non-kept old-path))
  (when (and projectile-mode
             (projectile-project-p)
             (projectile-file-cached-p old-path (projectile-project-root)))
    (projectile-purge-file-from-cache old-path))
  (when (bound-and-true-p save-place-mode)
    (save-place-forget-unreadable-files)))

(defun doom--copy-file (old-path new-path &optional force-p)
  (let* ((new-path (expand-file-name new-path))
         (old-path (file-truename old-path))
         (new-path (apply #'expand-file-name
                          (if (or (directory-name-p new-path)
                                  (file-directory-p new-path))
                              (list (file-name-nondirectory old-path) new-path)
                            (list new-path))))
         (new-path-dir (file-name-directory new-path))
         (project-root (doom-project-root))
         (short-new-name (if (file-in-directory-p new-path project-root)
                             (file-relative-name new-path project-root)
                           (abbreviate-file-name new-path))))
    (unless (file-directory-p new-path-dir)
      (make-directory new-path-dir t))
    (when (buffer-modified-p)
      (save-buffer))
    (cond ((equal (file-truename old-path)
                  (file-truename new-path))
           (throw 'status 'overwrite-self))
          ((and (file-exists-p new-path)
                (not force-p)
                (not (y-or-n-p (format "File already exists at %s, overwrite?" short-new-name))))
           (throw 'status 'aborted))
          (t
           (copy-file old-path new-path t)
           short-new-name))))

;;;###autoload
(defun doom/delete-this-file (&optional path force-p)
  "Delete FILENAME (defaults to the file associated with current buffer) and
kills the buffer. If FORCE-P, force the deletion (don't ask for confirmation)."
  (interactive
   (list (file-truename (buffer-file-name))
         current-prefix-arg))
  (let* ((fbase (file-name-sans-extension (file-name-nondirectory path)))
         (buf (current-buffer)))
    (cond ((not (file-exists-p path))
           (error "File doesn't exist: %s" path))
          ((not (or force-p (y-or-n-p (format "Really delete %s?" fbase))))
           (message "Aborted")
           nil)
          (t
           (unwind-protect
               (progn (delete-file path) t)
             (let ((short-path (file-relative-name path (doom-project-root))))
               (if (file-exists-p path)
                   (error "Failed to delete %s" short-path)
                 ;; Ensures that windows displaying this buffer will be switched
                 ;; to real buffers (`doom-real-buffer-p')
                 (doom/kill-this-buffer-in-all-windows buf t)
                 (doom--forget-file path)
                 (message "Successfully deleted %s" short-path))))))))

;;;###autoload
(defun doom/copy-this-file (new-path &optional force-p)
  "Copy current buffer's file to NEW-PATH. If FORCE-P, overwrite the destination
file if it exists, without confirmation."
  (interactive "F")
  (pcase (catch 'status
           (when-let* ((dest (doom--copy-file (buffer-file-name) new-path force-p)))
             (message "File successfully copied to %s" dest)))
    (`overwrite-self (error "Cannot overwrite self"))
    (`aborted (message "Aborted"))
    (_ t)))

;;;###autoload
(defun doom/move-this-file (new-path &optional force-p)
  "Move current buffer's file to NEW-PATH. If FORCE-P, overwrite the destination
file if it exists, without confirmation."
  (interactive "FP")
  (pcase (catch 'status
           (let ((old-path (buffer-file-name))
                 (new-path (expand-file-name new-path)))
             (when-let* ((dest (doom--copy-file old-path new-path force-p)))
               (delete-file old-path)
               (kill-this-buffer)
               (find-file new-path)
               (doom--forget-file old-path new-path)
               (message "File successfully moved to %s" dest))))
    (`overwrite-self (error "Cannot overwrite self"))
    (`aborted (message "Aborted"))
    (_ t)))

