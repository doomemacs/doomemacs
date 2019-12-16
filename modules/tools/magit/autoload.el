;;; tools/magit/autoload.el -*- lexical-binding: t; -*-

;; HACK Magit complains loudly when it can't determine its own version, which is
;;      the case when magit is built through straight. The warning is harmless,
;;      however, so we just need it to shut up.
;;;###autoload
(advice-add #'magit-version :override #'ignore)

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
              '(display-buffer-same-window))))))


;;
;; Commands

(defun +magit--refresh-vc-in-buffer (buffer)
  (with-current-buffer buffer
    (when (and vc-mode (fboundp 'vc-refresh-state))
      (vc-refresh-state))
    (when (and (bound-and-true-p git-gutter-mode)
               (fboundp '+version-control|update-git-gutter))
      (+version-control|update-git-gutter))
    (setq +magit--vc-is-stale-p nil)))

;;;###autoload
(defvar-local +magit--vc-is-stale-p nil)

;;;###autoload
(defun +magit-refresh-vc-state-maybe-h ()
  "Update `vc' and `git-gutter' if out of date."
  (when +magit--vc-is-stale-p
    (+magit--refresh-vc-in-buffer (current-buffer))))

;;;###autoload
(add-hook 'doom-switch-buffer-hook #'+magit-refresh-vc-state-maybe-h)

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
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (if (get-buffer-window buffer)
            (+magit--refresh-vc-in-buffer buffer)
          (with-current-buffer buffer
            (setq +magit--vc-is-stale-p t)))))))

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
