;;; tools/magit/autoload.el -*- lexical-binding: t; -*-

;; HACK Magit complains loudly (but harmlessly) when it can't determine its own
;;      version in a sparse clone. This was fixed upstream in
;;      magit/magit@b1b2683, but only for macOS and Linux users. Windows doesn't
;;      support symlinks as unix knows them, so `magit-version' can't resolve
;;      its own repo's path.
;;;###autoload
(eval-when! IS-WINDOWS
  (defadvice! +magit--ignore-version-a (&optional print-dest)
    :override #'magit-version
    (when print-dest
      (defvar magit-git-debug)
      (princ (format "Magit (unknown), Git %s, Emacs %s, %s"
                     (or (let ((magit-git-debug
                                (lambda (err)
                                  (display-warning '(magit git) err :error))))
                           (magit-git-version t))
                         "(unknown)")
                     emacs-version
                     system-type)
             print-dest))
    nil))

;;;###autoload
(defun +magit-display-buffer-fn (buffer)
  "Same as `magit-display-buffer-traditional', except...

- If opened from a commit window, it will open below it.
- Magit process windows are always opened in small windows below the current.
- Everything else will reuse the same window."
  (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
    (display-buffer
     buffer (cond
             ((and (eq buffer-mode 'magit-status-mode)
                   (get-buffer-window buffer))
              '(display-buffer-reuse-window))
             ;; Any magit buffers opened from a commit window should open below
             ;; it. Also open magit process windows below.
             ((or (bound-and-true-p git-commit-mode)
                  (eq buffer-mode 'magit-process-mode))
              (let ((size (if (eq buffer-mode 'magit-process-mode)
                              0.35
                            0.7)))
                `(display-buffer-below-selected
                  . ((window-height . ,(truncate (* (window-height) size)))))))

             ;; Everything else should reuse the current window.
             ((or (not (derived-mode-p 'magit-mode))
                  (not (memq (with-current-buffer buffer major-mode)
                             '(magit-process-mode
                               magit-revision-mode
                               magit-diff-mode
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
    (if-let (window (window-in-direction direction))
        (select-window window)
      (if-let (window (and (not (one-window-p))
                           (window-in-direction
                            (pcase direction
                              (`right 'left)
                              (`left 'right)
                              ((or `up `above) 'down)
                              ((or `down `below) 'up)))))
          (select-window window)
        (let ((window (split-window nil nil direction)))
          (when (memq direction '(right down below))
            (select-window window))
          (display-buffer-record-window 'reuse window buffer)
          (set-window-buffer window buffer)
          (set-window-parameter window 'quit-restore (list 'window 'window origin-window buffer))
          (set-window-prev-buffers window nil))))
    (switch-to-buffer buffer t t)
    (selected-window)))


;;
;;; Auto-revert

(defvar +magit--stale-p nil)

(defun +magit--revert-buffer (buffer)
  (with-current-buffer buffer
    (kill-local-variable '+magit--stale-p)
    (when buffer-file-name
      (if (buffer-modified-p (current-buffer))
          (when (bound-and-true-p vc-mode)
            (vc-refresh-state)
            (force-mode-line-update))
        (revert-buffer t t)))))

;;;###autoload
(defun +magit-mark-stale-buffers-h ()
  "Revert all visible buffers and mark buried buffers as stale.

Stale buffers are reverted when they are switched to, assuming they haven't been
modified."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (if (get-buffer-window buffer)
          (+magit--revert-buffer buffer)
        (with-current-buffer buffer
          (setq-local +magit--stale-p t))))))

;;;###autoload
(defun +magit-revert-buffer-maybe-h ()
  "Update `vc' and `git-gutter' if out of date."
  (when +magit--stale-p
    (+magit--revert-buffer (current-buffer))))


;;
;;; Commands

;;;###autoload
(defun +magit/quit (&optional kill-buffer)
  "Clean up magit buffers after quitting `magit-status' and refresh version
control in buffers."
  (interactive "P")
  (funcall magit-bury-buffer-function kill-buffer)
  (unless (delq nil
                (mapcar (lambda (win)
                          (with-selected-window win
                            (eq major-mode 'magit-status-mode)))
                        (window-list)))
    (mapc #'+magit--kill-buffer (magit-mode-get-buffers))
    (+magit-mark-stale-buffers-h)))

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
(defun +magit/start-github-review (arg)
  (interactive "P")
  (call-interactively
    (if (or arg (not (featurep 'forge)))
        #'github-review-start
      #'github-review-forge-pr-at-point)))

(defvar +magit-clone-history nil
  "History for `+magit/clone' prompt.")
;;;###autoload
(defun +magit/clone (url-or-repo dir)
  "Like `magit-clone', but supports additional formats on top of absolute URLs:

+ USER/REPO: assumes {`+magit-default-clone-url'}/USER/REPO
+ REPO: assumes {`+magit-default-clone-url'}/{USER}/REPO, where {USER} is
  ascertained from your global gitconfig."
  (interactive
   (progn
     (require 'ghub)
     (let* ((user (ghub--username (ghub--host)))
            (repo (read-from-minibuffer
                   "Clone repository (user/repo or url): "
                   (if user (concat user "/"))
                   nil nil '+magit-clone-history))
            (name (car (last (split-string repo "/" t)))))
       (list repo
             (read-directory-name
              "Destination: "
              magit-clone-default-directory
              name nil name)))))
  (magit-clone-regular
   (cond ((string-match-p "^[^/]+$" url-or-repo)
          (require 'ghub)
          (format +magit-default-clone-url (ghub--username (ghub--host)) url-or-repo))
         ((string-match-p "^\\([^/]+\\)/\\([^/]+\\)/?$" url-or-repo)
          (apply #'format +magit-default-clone-url (split-string url-or-repo "/" t)))
         (url-or-repo))
   dir
   nil))
