;;; feature/evil/autoload/files.el

(defun doom--forget-file (old-path &optional new-path)
  "Ensure `recentf', `projectile' and `save-place' forget OLD-PATH."
  (when (fboundp 'recentf-add-file)
    (when new-path
      (recentf-add-file new-path))
    (recentf-remove-if-non-kept old-path))
  (when (and (projectile-project-p)
             (projectile-file-cached-p old-path (projectile-project-root)))
    (projectile-purge-file-from-cache old-path))
  (when (bound-and-true-p save-place-mode)
    (save-place-forget-unreadable-files)))

;;;###autoload (autoload '+evil:delete-this-file "feature/evil/autoload/files" nil t)
(evil-define-command +evil:delete-this-file (&optional filename force-p)
  "Delete FILENAME (defaults to the file associated with current buffer) and
kills the buffer. If FORCE-P, force the deletion (don't ask for confirmation)."
  :repeat nil
  (interactive "<f><!>")
  (let* ((fname (file-truename (or filename (buffer-file-name))))
         (fbase (file-name-sans-extension (file-name-nondirectory fname)))
         (buf (current-buffer)))
    (cond ((not (file-exists-p fname))
           (error "File doesn't exist: %s" fname))

          ((not (or force-p (y-or-n-p (format "Really delete %s?" fbase))))
           (message "Aborted"))

          (t
           (unwind-protect
               (delete-file fname)
             (let ((short-path (file-relative-name fname (doom-project-root))))
               (if (file-exists-p fname)
                   (error "Failed to delete %s" short-path)
                 ;; Ensures that windows displaying this buffer will be switched
                 ;; to real buffers (`doom-real-buffer-p')
                 (doom-force-kill-buffer buf t)
                 (doom--forget-file fname)
                 (message "Successfully deleted %s" short-path))))))))

;;;###autoload (autoload '+evil:move-this-file "feature/evil/autoload/files" nil t)
(evil-define-command +evil:move-this-file (new-path &optional force-p)
  "Move current buffer's file to NEW-PATH. Replaces %, # and other vim-esque
filename modifiers (see `+evil*ex-replace-special-filenames'). If FORCE-P,
overwrite the destination file if it exists, without confirmation."
  :repeat nil
  (interactive "<f><!>")
  (let* ((new-path (expand-file-name new-path))
         (old-path (file-truename (buffer-file-name)))
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
           (error "Cannot move file to itself"))
          ((and (file-exists-p new-path)
                (not force-p)
                (not (y-or-n-p (format "File already exists at %s, overwrite?" short-new-name))))
           (message "Aborted"))
          (t
           (rename-file old-path new-path 1)
           (kill-this-buffer)
           (find-file new-path)
           (doom--forget-file old-path new-path)
           (message "File successfully moved to %s"
                    short-new-name)))))


