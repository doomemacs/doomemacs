;;; feature/evil/autoload/files.el

;;;###autoload (autoload '+evil:file-delete "feature/evil/autoload/files" nil t)
(evil-define-command +evil:file-delete (&optional bang filename)
  "Delete current buffer's file. If BANG, don't ask for confirmation."
  :repeat nil
  ;; TODO Test me
  (interactive "<!><f>")
  (let* ((fname (file-truename (or fname (buffer-file-name))))
         (fbase (file-name-sans-extension (file-name-nondirectory fname)))
         (buf (current-buffer)))
    (cond ((not (file-exists-p fname))
           (error "File doesn't exist: %s" fname))

          ((not (or bang (y-or-n-p (format "Delete %s?" fbase))))
           (message "Aborted"))

          (t
           (unwind-protect
               (delete-file fname)
             (if (file-exists-p fname)
                 (error "Failed to delete %s" (file-relative-name fname))
               (doom/previous-real-buffer)
               (kill-buffer buf)
               (when (bound-and-true-p save-place-mode)
                 (save-place-forget-unreadable-files))
               (message "Successfully deleted %s" (file-relative-name fname))))))))

(defun doom--save-exit() (save-buffer) (kill-buffer) (remove-hook 'yas-after-exit-snippet-hook '--save-exit))
;;;###autoload (autoload '+evil:file-create "feature/evil/autoload/files" nil t)
(evil-define-command +evil:file-create (path &optional bang)
  "Deploy files (and their associated templates) quickly. Will prompt
you to fill in each snippet field before buffer closes unless BANG is
provided."
  :repeat nil
  ;; TODO Test me
  (interactive "<f><!>")
  (let* ((fullpath (expand-file-name path))
         (dir (file-name-directory fullpath))
         (is-auto t))
    (when (and bang (not (file-exists-p dir)))
      (mkdir dir))
    (if (file-exists-p dir)
        (if (file-exists-p fullpath)
            (error "File already exists: %s" path)
          (find-file fullpath)
          (add-hook 'yas-after-exit-snippet-hook 'doom--save-exit)
          (if bang (doom--save-exit)))
      (error "Directory doesn't exist: %s" dir))))

;;;###autoload (autoload '+evil:file-move "feature/evil/autoload/files" nil t)
(evil-define-command +evil:file-move (bang dest-path)
  "Move current buffer's file to PATH. Replaces %, # and other variables (see
`evil-ex-replace-special-filenames')"
  :repeat nil
  ;; TODO Test me
  (interactive "<!><f>")
  (let* ((dest-path (expand-file-name dest-path))
         (old-path (file-truename (buffer-file-name)))
         (new-path (cond ((file-directory-p dest-path)
                          (expand-file-name (file-name-nondirectory old-path) dest-path))
                         ((file-directory-p (file-name-directory dest-path))
                          (expand-file-name dest-path))
                         (t (user-error "Not a valid destination: %s" dest-path))))
         (project-root (doom-project-root))
         (short-old-path (file-relative-name old-path project-root))
         (short-new-path (file-relative-name new-path project-root)))
    (when (y-or-n-p (format "Renaming %s to %s, proceed?"
                            short-old-path short-dest-path))
      (when (buffer-modified-p)
        (save-buffer))
      (rename-file old-path new-path 1)
      (rename-buffer (file-name-nondirectory new-path))
      (set-visited-file-name new-path)
      (set-buffer-modified-p nil)
      (save-place-forget-unreadable-files)
      (setq doom--spaceline-file-path nil)
      (message "File '%s' successfully renamed to '%s'"
               short-old-path short-new-path))))

