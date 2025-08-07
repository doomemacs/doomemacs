;;; tools/magit/autoload.el -*- lexical-binding: t; -*-

;; HACK Magit complains loudly (but harmlessly) when it can't determine its own
;;      version in a sparse clone. Since I'd rather not compromise on shallow
;;      clones, I've gimped `magit-version' so it doesn't complain (unless
;;      called interactively).
;;;###autoload
(defadvice! +magit--ignore-version-a (fn &rest args)
  :around #'magit-version
  (let ((inhibit-message (not (called-interactively-p 'any))))
    (apply fn args)))

;;;###autoload
(defun +magit-display-buffer-fn (buffer)
  "Same as `magit-display-buffer-traditional', except...

- If opened from a commit window, it will open below it.
- Magit process windows are always opened in small windows below the current.
- Everything else will reuse the same window."
  (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
    (display-buffer
     buffer (cond
             ;; Prevent opening multiple status windows for the same project.
             ((and (eq buffer-mode 'magit-status-mode)
                   (get-buffer-window buffer))
              '(display-buffer-reuse-window))

             ((or
               ;; Any magit buffers opened from a commit window should open
               ;; below the selected one.
               (bound-and-true-p git-commit-mode)
               ;; ...Or process log buffers...
               (eq buffer-mode 'magit-process-mode)
               ;; Nothing should replace the log-select buffer (revision buffers
               ;; during rebasing, in particular).
               (eq major-mode 'magit-log-select-mode))
              (let ((size (if (eq buffer-mode 'magit-process-mode)
                              0.35
                            0.7)))
                `(display-buffer-below-selected
                  . ((window-height . ,(truncate (* (window-height) size)))))))

             ((or
               ;; If triggered from outside of magit, open magit in the current
               ;; window, rather than a far-away one.
               (not (derived-mode-p 'magit-mode))
               ;; If invoking a diff from the status buffer, use that window.
               (and (eq major-mode 'magit-status-mode)
                    (memq buffer-mode
                          '(magit-diff-mode
                            magit-stash-mode)))
               ;; These buffers should open in another (but nearby) window,
               ;; because they compliment the current one being visible.
               (not (memq buffer-mode
                          '(magit-process-mode
                            magit-revision-mode
                            magit-stash-mode
                            magit-status-mode))))
              '(display-buffer-same-window))

             ('(+magit--display-buffer-in-direction))))))

(defun +magit--display-buffer-in-direction (buffer alist)
  "`display-buffer-alist' handler that opens BUFFER in a direction.

This differs from `display-buffer-in-direction' in one way: it will try to use a
window that already exists in that direction. It will split otherwise."
  (let ((direction (or (alist-get 'direction alist)
                       +magit-open-windows-in-direction))
        (origin-window (selected-window)))
    (if-let* ((window (window-in-direction direction)))
        (unless magit-display-buffer-noselect
          (select-window window))
      (if-let* ((window (and (not (one-window-p))
                             (window-in-direction
                              (pcase direction
                                (`right 'left)
                                (`left 'right)
                                ((or `up `above) 'down)
                                ((or `down `below) 'up))))))
        (unless magit-display-buffer-noselect
          (select-window window))
        (let ((window (split-window nil nil direction)))
          (when (and (not magit-display-buffer-noselect)
                     (memq direction '(right down below)))
            (select-window window))
          (display-buffer-record-window 'reuse window buffer)
          (set-window-buffer window buffer)
          (set-window-parameter window 'quit-restore (list 'window 'window origin-window buffer))
          (set-window-prev-buffers window nil))))
    (unless magit-display-buffer-noselect
      (switch-to-buffer buffer t t)
      (selected-window))))


;;
;;; Auto-revert

(defvar +magit--stale-p nil)

(defun +magit--revertable-buffer-p (buffer)
  (when (buffer-live-p buffer)
    (pcase +magit-auto-revert
      (`t t)
      (`local
       (not (file-remote-p
             (or (buffer-file-name buffer)
                 (buffer-local-value 'default-directory buffer)))))
      ((pred functionp)
       (funcall +magit-auto-revert buffer)))))

(defun +magit--revert-buffer (buffer)
  (with-current-buffer buffer
    (kill-local-variable '+magit--stale-p)
    (when (magit-auto-revert-repository-buffer-p buffer)
      (save-restriction
        (cl-incf magit-auto-revert-counter)
        (when (bound-and-true-p vc-mode)
          (let ((vc-follow-symlinks t))
            (vc-refresh-state))
          (when (fboundp '+vc-gutter-update-h)
            (+vc-gutter-update-h)))
        (when (and (not (get-buffer-process buffer))
                   (funcall buffer-stale-function t))
          (revert-buffer t t t))
        (force-mode-line-update)))))

;;;###autoload
(defun +magit-mark-stale-buffers-h ()
  "Revert all visible buffers and mark buried buffers as stale.

Stale buffers are reverted when they are switched to, assuming they haven't been
modified."
  (when +magit-auto-revert
    (let ((visible-buffers (doom-visible-buffers nil t)))
      (dolist (buffer (buffer-list))
        (when (+magit--revertable-buffer-p buffer)
          (if (memq buffer visible-buffers)
              (progn
                (+magit--revert-buffer buffer)
                (cl-callf2 delq buffer visible-buffers)) ; hasten future lookups
            (with-current-buffer buffer
              (setq-local +magit--stale-p t))))))))

;;;###autoload
(defun +magit-revert-buffer-maybe-h ()
  "Update `vc' and `diff-hl' if out of date."
  (when +magit--stale-p
    (+magit--revert-buffer (current-buffer))))


;;
;;; Commands

;;;###autoload
(defun +magit/quit (&optional kill-buffer)
  "Bury the current magit buffer.

If KILL-BUFFER, kill this buffer instead of burying it.
If the buried/killed magit buffer was the last magit buffer open for this repo,
kill all magit buffers for this repo."
  (interactive "P")
  (let ((topdir (magit-toplevel)))
    (funcall magit-bury-buffer-function kill-buffer)
    (or (cl-find-if (lambda (win)
                      (with-selected-window win
                        (and (derived-mode-p 'magit-mode)
                             (equal magit--default-directory topdir))))
                    (window-list))
        (+magit/quit-all))))

;;;###autoload
(defun +magit/quit-all ()
  "Kill all magit buffers for the current repository."
  (interactive)
  (mapc #'+magit--kill-buffer (magit-mode-get-buffers))
  (+magit-mark-stale-buffers-h))

(defun +magit--kill-buffer (buf)
  "TODO"
  (when (and (bufferp buf) (buffer-live-p buf))
    (let ((process (get-buffer-process buf)))
      (if (not (processp process))
          (kill-buffer buf)
        (with-current-buffer buf
          (if (process-live-p process)
              (run-with-timer 5 nil #'+magit--kill-buffer buf)
            (kill-process process)
            (kill-buffer buf)))))))

;;;###autoload
(defun +magit/start-code-review (arg)
  (interactive "P")
  (call-interactively
    (if (or arg (not (featurep 'forge)))
        #'code-review-start
      #'code-review-forge-pr-at-point)))
