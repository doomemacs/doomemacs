;;; tools/magit/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +magit-display-buffer-fullscreen (buffer)
  "Like `magit-display-buffer-fullframe-status-v1' with two differences:

1. Magit sub-buffers that aren't spawned from a status screen are opened as
   popups.
2. The status screen isn't buried when viewing diffs or logs from the status
   screen."
  (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
    (display-buffer
     buffer (cond
             ;; If opened from an eshell window, use the same window.
             ((derived-mode-p 'eshell-mode)
              '(display-buffer-same-window))
             ;; If opened from a magit window from a popup, open the results
             ;; full screen. We want to see it all.
             ((eq (window-dedicated-p) 'side)
              '(magit--display-buffer-fullframe))
             ;; From a commit or magit-log buffer, open detail buffers below
             ;; this one.
             ((or (bound-and-true-p git-commit-mode)
                  (derived-mode-p 'magit-log-mode))
              '(display-buffer-below-selected . ((window-height . 0.7))))
             ;; From a magit buffer, set aside the magit-status window if it
             ;; exists (we want it always to be visible), then display the
             ;; target buffer in the current window.
             ((derived-mode-p 'magit-mode)
              (when (eq major-mode 'magit-status-mode)
                (display-buffer-in-side-window
                 (current-buffer) '((side . left)
                                    (window-width . 0.35)
                                    (window-parameters (quit)))))
              '(display-buffer-same-window))
             ;; If the target buffer opening is a commit, revision or diff, we
             ;; want to see the whole thing.
             ((or (buffer-local-value 'git-commit-mode buffer)
                  (memq buffer-mode '(magit-revision-mode magit-diff-mode)))
              '(magit--display-buffer-fullframe))
             ;; log/stash/process buffers, unless opened from a magit-status
             ;; window, should be opened in popups.
             ((memq buffer-mode '(magit-process-mode
                                  magit-log-mode
                                  magit-stash-mode))
              '(display-buffer-in-side-window))
             ;; Last resort: plain old fullscreen.
             ('(magit--display-buffer-fullframe))))))


;;
;; Commands
;;

;;;###autoload
(defun +magit/quit (&optional _kill-buffer)
  "Clean up magit buffers after quitting `magit-status'."
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (mapc #'+magit--kill-buffer buffers)))

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
  "Delegates to `magit-clone' or `magithub-clone' depending on the repo url
format."
  (interactive
   (progn
     (require 'magithub)
     (let* ((user (ghubp-username))
            (repo (read-from-minibuffer
                   "Clone repository (user/repo or url): "
                   (if user (concat user "/"))
                   nil nil '+magit-clone-history))
            (name (car (last (split-string repo "/" t)))))
       (list repo
             (read-directory-name
              "Destination: "
              magithub-clone-default-directory
              name nil name)))))
  (require 'magithub)
  (if (string-match "^\\([^/]+\\)/\\([^/]+\\)$" url-or-repo)
      (let ((repo `((owner (login . ,(match-string 1 url-or-repo)))
                    (name . ,(match-string 2 url-or-repo)))))
        (and (or (magithub-request
                  (ghubp-get-repos-owner-repo repo))
                 (let-alist repo
                   (user-error "Repository %s/%s does not exist"
                               .owner.login .name)))
             (magithub-clone repo dir)))
    (magit-clone url-or-repo dir)))


;;
;; Advice
;;

;;;###autoload
(defun +magit*hub-settings--format-magithub.enabled ()
  "Change the setting to display 'false' as its default."
  (magit--format-popup-variable:choices "magithub.enabled" '("true" "false") "false"))

;;;###autoload
(defun +magit*hub-enabled-p ()
  "Disables magithub by default."
  (magithub-settings--value-or "magithub.enabled" nil
    #'magit-get-boolean))
