;; Inspired by http://demonastery.org/2013/04/emacs-evil-narrow-region/
;;;###autoload
(defun narf:narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

;;;###autoload
(defun narf:widen ()
  (interactive)
  (when (buffer-narrowed-p)
    (widen)))

;;;###autoload
(defun narf:set-region-read-only (begin end)
  "See http://stackoverflow.com/questions/7410125"
  (let ((modified (buffer-modified-p)))
    (add-text-properties begin end '(read-only t))
    (set-buffer-modified-p modified)))

;;;###autoload
(defun narf:set-region-writeable (begin end)
  "See http://stackoverflow.com/questions/7410125"
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t))
    (remove-text-properties begin end '(read-only t))
    (set-buffer-modified-p modified)))

;;;###autoload
(defun narf/living-buffer-list (&optional buffer-list)
  (-remove 'get-buffer-window (or buffer-list (buffer-list))))


;; Killing Buffers ;;;;;;;;;;;;;;;;;;;;;
;; Buffer defuns
(defvar narf/cleanup-buffers-list '("^ \\*"
                                    "^\\*Backtrace\\*$"
                                    "^\\*Warnings\\*$"
                                    "^\\*Compile-Log\\*$"
                                    "^\\*Ediff.*\\*$"
                                    help-mode
                                    image-mode
                                    dired-mode
                                    reb-mode)
  "A list of buffer name regexps or major-mode symbols. If buried buffers
  match/have that mode active, `narf:cleanup-buffers' will kill them.")

(defvar narf/cleanup-processes-alist '(("pry" . ruby-mode)
                                       ("irb" . ruby-mode)
                                       ("ipython" . python-mode))
  "An alist of (process-name . major-mode), that `narf:cleanup-processes' checks
before killing processes. If there are no buffers with matching major-modes, it
gets killed.")

;;;###autoload
(defun narf/add-throwaway-buffer (regexp)
  (add-to-list 'narf/cleanup-buffers-list regexp))

;;;###autoload
(defun narf:cleanup-buffers ()
  "Kill left-over temporary, dired or buried special buffers"
  (interactive)
  (let* ((this-frame (selected-frame))
         (kill-list (buffer-list this-frame)))
    (setq kill-list
          (-filter (lambda (b)
                     (unless (get-buffer-window b) ; don't kill if visible
                       (-any? (lambda (pred)
                                (cond ((stringp pred)
                                       (s-matches? pred (buffer-name b)))
                                      ((symbolp pred)
                                       (eq (buffer-local-value 'major-mode b) pred))))
                              narf/cleanup-buffers-list)))
                   kill-list))
    (message "Cleaned up %s buffers" (length kill-list))
    (mapc 'kill-buffer kill-list)
    (narf:cleanup-processes)))

;;;###autoload
(defun narf:cleanup-processes ()
  (interactive)
  (let ((buffer-list (buffer-list)))
    (dolist (p (process-list))
      (let* ((process-name (process-name p))
             (assoc (assoc process-name narf/cleanup-processes-alist)))
        (when (and assoc
                   (not (string= process-name "server"))
                   (process-live-p p)
                   (not (-any? (lambda (b)
                                 (let ((mode (buffer-local-value 'major-mode b)))
                                   (eq mode (cdr assoc))))
                               buffer-list)))
          (message "Cleanup: killing %s" process-name)
          (delete-process p))))))

;;;###autoload
(defun narf:kill-matching-buffers (regexp &optional buffer-list)
  (interactive)
  (mapc (lambda (b)
          (if (string-match-p regexp (buffer-name b))
              (kill-buffer b)))
        (if buffer-list buffer-list (buffer-list))))

(defun narf--cycle-real-buffer (&optional n)
  "Switch to the previous buffer and avoid special buffers."
  (interactive)
  (let ((start-buffer (current-buffer))
        (move-func (if (< n 0) 'switch-to-next-buffer 'switch-to-prev-buffer)))
    (funcall move-func)
    (let ((i 0))
      (while (let ((current-buffer (current-buffer)))
               (and (not (eq current-buffer start-buffer))
                    (not (eq current-buffer narf--project-scratch-buffer))
                    (< i 15)
                    (string-equal "*" (substring (buffer-name) 0 1))))
        (cl-incf i)
        (funcall move-func)))))

;; From spacemacs <https://github.com/syl20bnr/spacemacs/blob/master/spacemacs/funcs.el>
;;;###autoload
(defun narf:next-real-buffer ()
  "Switch to the next buffer and avoid special buffers."
  (interactive)
  (narf--cycle-real-buffer +1))

;;;###autoload
(defun narf:previous-real-buffer ()
  "Switch to the previous buffer and avoid special buffers."
  (interactive)
  (narf--cycle-real-buffer -1))

;;;###autoload
(defun narf:kill-real-buffer ()
  "Kill buffer (but only bury scratch buffer)"
  (interactive)
  (let ((bname (buffer-name)))
    (cond ((string-match-p "^\\*scratch\\*" bname)
           (erase-buffer)
           (bury-buffer))
          ((string-equal "*" (substring bname 0 1))
           (previous-buffer))
          (t (kill-this-buffer)))))

;;;###autoload (autoload 'narf::save-session "defuns-buffers")
(evil-define-command narf::save-session (&optional bang session-name)
  (interactive "<!><a>")
  (if session-name
      (wg-save-session-as (concat wg-workgroup-directory session-name) (not bang))
    (wg-save-session)))

;;;###autoload (autoload 'narf::load-session "defuns-buffers")
(evil-define-command narf::load-session (&optional bang session-name)
  (interactive "<!><a>")
  (wg-open-session (if session-name
                       (concat wg-workgroup-directory session-name)
                     wg-session-file)))

;;;###autoload (autoload 'narf::new-workgroup "defuns-buffers")
(evil-define-command narf::new-workgroup (bang name)
  (interactive "<!><a>")
  (unless name
    (user-error "No name specified for new workgroup"))
  (if bang
      (wg-clone-workgroup (wg-current-workgroup) name)
    (wg-create-workgroup name t)))

;;;###autoload (autoload 'narf::rename-workgroup "defuns-buffers")
(evil-define-command narf::rename-workgroup (new-name)
  (interactive "<a>")
  (wg-rename-workgroup new-name))

;;;###autoload (autoload 'narf::rename-this-file "defuns-buffers")
(evil-define-command narf::rename-this-file (new-name)
  "Renames current buffer and file it is visiting. Replaces %, # and other
  variables (see `evil-ex-replace-special-filenames')"
  :repeat nil
  (interactive "<f>")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name
             (expand-file-name
              (evil-ex-replace-special-filenames (if new-name
                                                     new-name
                                                   (read-file-name "New name: " filename)))
              (f-dirname filename))))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (save-place-forget-unreadable-files)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

;;;###autoload (autoload 'narf::delete-this-file "defuns-buffers")
(evil-define-command narf::delete-this-file (&optional bang)
  "Delete current buffer's file. If bang, then kill the buffer afterwards as well."
  :repeat nil
  (interactive "<!>")
  (let ((filename (file-truename (buffer-file-name))))
    (if (not (file-exists-p filename))
        (error "File doesn't exist: %s" filename)
      (delete-file filename)
      (when bang (kill-this-buffer))
      (save-place-forget-unreadable-files)
      (message "File successfully deleted: %s" filename))))

(defun narf--save-exit() (save-buffer) (kill-buffer) (remove-hook 'yas-after-exit-snippet-hook '--save-exit))
;;;###autoload (autoload 'narf::create-file "defuns-buffers")
(evil-define-command narf::create-file (path &optional bang)
  "Deploy files (and their associated templates) quickly. Will prompt
you to fill in each snippet field before buffer closes unless BANG is
provided."
  :repeat nil
  (interactive "<f><!>")
  (let ((dir (f-dirname path))
        (fullpath (f-full path))
        (is-auto t))
    (when (and bang (not (file-exists-p dir))) (f-mkdir dir))
    (if (file-exists-p dir)
        (if (file-exists-p fullpath)
            (error "File already exists: %s" path)
          (find-file fullpath)
          (add-hook 'yas-after-exit-snippet-hook 'narf--save-exit)
          (if bang (--save-exit)))
      (error "Directory doesn't exist: %s" dir))))

;;;###autoload (autoload 'narf::scratch-buffer "defuns-buffers")
(evil-define-operator narf::scratch-buffer (&optional beg end bang)
  "Send a selection to the scratch buffer. If BANG, then send it to org-capture
  instead."
  :move-point nil
  :type inclusive
  (interactive "<r><!>")
  (let ((mode major-mode)
        (text (when (and (evil-visual-state-p) beg end)
                (buffer-substring beg end))))
    (if bang
        ;; use org-capture with bang
        (if text
            (org-capture-string text)
          (org-capture))
      ;; or scratch buffer by default
      (let* ((project-dir (narf/project-root t))
             (buffer-name (if project-dir
                              (format "*scratch* (%s)" (narf/project-name))
                            "*scratch*")))
        (popwin:popup-buffer (get-buffer-create buffer-name))
        (when (eq (get-buffer buffer-name) (current-buffer))
          (when project-dir
            (cd project-dir))
          (if text (insert text))
          (funcall mode))))))

;;;###autoload (autoload 'narf::kill-buried-buffers "defuns-buffers")
(evil-define-command narf::kill-buried-buffers (&optional bang)
  :repeat nil
  (interactive "<!>")
  (mapc 'kill-buffer
        (narf/living-buffer-list (if bang (projectile-project-buffers) (buffer-list)))))

;;;###autoload (autoload 'narf::kill-buffers "defuns-buffers")
(evil-define-command narf::kill-buffers (&optional bang)
  :repeat nil
  (interactive "<!>")
  (if (and (not bang) (projectile-project-p))
      (projectile-kill-buffers)
    (mapc 'kill-buffer (buffer-list)))
  (delete-other-windows)
  (switch-to-buffer (if (buffer-live-p narf--project-scratch-buffer)
                        narf--project-scratch-buffer
                      (get-buffer-create "*scratch*"))))

;;;###autoload (autoload 'narf::cd "defuns-buffers")
(evil-define-command narf::cd (dir)
  :repeat nil
  (interactive "<f>")
  (cd (if (zerop (length dir)) "~" dir)))


(provide 'defuns-buffers)
;;; defuns-buffers.el ends here
