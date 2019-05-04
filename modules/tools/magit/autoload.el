;;; tools/magit/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +magit-display-buffer (buffer)
  "Marries `magit-display-buffer-fullcolumn-most-v1' with
`magit-display-buffer-same-window-except-diff-v1', except:

1. Magit sub-buffers that aren't spawned from a status screen are opened as
   popups.
2. The status screen isn't buried when viewing diffs or logs from the status
   screen."
  (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
    (display-buffer
     buffer (cond
             ;; If opened from an eshell window or popup, use the same window.
             ((or (derived-mode-p 'eshell-mode)
                  (eq (window-dedicated-p) 'side))
              '(display-buffer-same-window))
             ;; Open target buffers below the current one (we want previous
             ;; magit windows to be visible; especially magit-status).
             ((or (bound-and-true-p git-commit-mode)
                  (derived-mode-p 'magit-mode))
              (let ((size (if (eq buffer-mode 'magit-process-mode)
                              0.35
                            0.7)))
                `(display-buffer-below-selected
                  . ((window-height . ,(truncate (* (window-height) size)))))))
             ;; log/stash/process buffers, unless opened from a magit-status
             ;; window, should be opened in popups.
             ((memq buffer-mode '(magit-process-mode
                                  magit-log-mode
                                  magit-stash-mode))
              '(display-buffer-below-selected))
             ;; Last resort: use current window
             ('(display-buffer-same-window))))))


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
(defun +magit|refresh-vc-state-maybe ()
  "Update `vc' and `git-gutter' if out of date."
  (when +magit--vc-is-stale-p
    (+magit--refresh-vc-in-buffer (current-buffer))))

;;;###autoload
(add-hook 'doom-switch-buffer-hook #'+magit|refresh-vc-state-maybe)

;;;###autoload
(defun +magit/quit (&optional _kill-buffer)
  "Clean up magit buffers after quitting `magit-status' and refresh version
control in buffers."
  (interactive)
  (quit-window)
  (unless (cdr
           (delq nil
                 (mapcar (lambda (win)
                           (with-selected-window win
                             (eq major-mode 'magit-status-mode)))
                         (window-list))))
    (mapc #'+magit--kill-buffer (magit-mode-get-buffers))
    (dolist (buffer (doom-buffer-list))
      (if (get-buffer-window buffer)
          (+magit--refresh-vc-in-buffer buffer)
        (with-current-buffer buffer
          (setq +magit--vc-is-stale-p t))))))

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
  (magit-clone
   (cond ((string-match-p "^[^/]+$" url-or-repo)
          (require 'ghub)
          (format +magit-default-clone-url (ghub--username (ghub--host)) url-or-repo))
         ((string-match-p "^\\([^/]+\\)/\\([^/]+\\)/?$" url-or-repo)
          (apply #'format +magit-default-clone-url (split-string url-or-repo "/" t)))
         (url-or-repo))
   dir))


;;
;; Advice

;;;###autoload
(defun +magit*hub-settings--format-magithub.enabled ()
  "Change the setting to display 'false' as its default."
  (magit--format-popup-variable:choices "magithub.enabled" '("true" "false") "false"))

;;;###autoload
(defun +magit*hub-enabled-p ()
  "Disables magithub by default."
  (magithub-settings--value-or "magithub.enabled" nil
    #'magit-get-boolean))
