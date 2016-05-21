;;; defuns-file.el

;;;###autoload (autoload 'doom:file-delete "defuns-file" nil t)
(evil-define-command doom:file-delete (&optional bang filename)
  "Delete current buffer's file. If bang, then kill the buffer afterwards as well."
  :repeat nil
  (interactive "<!><f>")
  (let ((filename (file-truename (or filename (buffer-file-name)))))
    (if (not (file-exists-p filename))
        (error "File doesn't exist: %s" filename)
      (when (or bang (and (not bang) (y-or-n-p (format "Delete %s?" (f-base filename)))))
        ;; Delete attachments if in org-mode
        (when (eq major-mode 'org-mode)
          (mapc (lambda (file)
                  (when (file-exists-p file)
                    (delete-file file t)))
                (doom/org-attachments)))
        (set-buffer-modified-p nil)
        (delete-file filename)
        (kill-this-buffer)
        (unless (doom/real-buffer-p)
          (doom/previous-real-buffer))
        (save-place-forget-unreadable-files)
        (message "File successfully deleted: %s" filename)))))

(defun doom--save-exit() (save-buffer) (kill-buffer) (remove-hook 'yas-after-exit-snippet-hook '--save-exit))
;;;###autoload (autoload 'doom:file-create "defuns-file" nil t)
(evil-define-command doom:file-create (path &optional bang)
  "Deploy files (and their associated templates) quickly. Will prompt
you to fill in each snippet field before buffer closes unless BANG is
provided."
  :repeat nil
  (interactive "<f><!>")
  (let ((dir (f-dirname path))
        (fullpath (f-full path))
        (is-auto t))
    (when (and bang (not (f-exists? dir)))
      (mkdir dir))
    (if (f-exists? dir)
        (if (f-exists? fullpath)
            (error "File already exists: %s" path)
          (find-file fullpath)
          (add-hook 'yas-after-exit-snippet-hook 'doom--save-exit)
          (if bang (doom--save-exit)))
      (error "Directory doesn't exist: %s" dir))))

;;;###autoload (autoload 'doom:file-move "defuns-file" nil t)
(evil-define-command doom:file-move (path)
  "Move current buffer's file to PATH. Replaces %, # and other variables (see
  `evil-ex-replace-special-filenames')"
  :repeat nil
  (interactive "<f>")
  (let* ((old-path (buffer-file-name))
         (new-path (cond ((f-dir? path)
                          (f-expand (f-filename old-path) path))
                         ((f-dir? (f-dirname path))
                          (f-full path))
                         (t (user-error "Not a valid destination: %s" path))))
         (project-root (doom/project-root)))
    ;; Move all attachments if in org-mode
    (when (eq major-mode 'org-mode)
      (mapc (lambda (file)
              (when (and (file-exists-p file) (not (f-same? old-path new-path)))
                (rename-file file (f-expand (f-filename old-path) (f-dirname new-path)) t)))
            (doom/org-attachments)))
    (when (buffer-modified-p)
      (save-buffer))
    (rename-file old-path new-path 1)
    (rename-buffer (f-filename new-path))
    (set-visited-file-name new-path)
    (set-buffer-modified-p nil)
    (save-place-forget-unreadable-files)
    (setq doom--spaceline-file-path nil)
    (message "File '%s' successfully renamed to '%s'"
             (f-relative old-path project-root) (f-relative new-path project-root))))

(provide 'defuns-file)
;;; defuns-file.el ends here
