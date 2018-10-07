;;; tools/magit/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +magit-display-buffer (buffer)
  "Like `magit-display-buffer-fullframe-status-v1' with two differences:

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

;;;###autoload
(defun +magit-display-popup-buffer (buffer &optional alist)
  "TODO"
  (cond ((eq (window-dedicated-p) 'side)
         (if (fboundp '+popup-display-buffer-stacked-side-window)
             (+popup-display-buffer-stacked-side-window buffer alist)
           (display-buffer-in-side-window buffer alist)))
        ((derived-mode-p 'magit-mode)
         (display-buffer-below-selected buffer alist))
        ((display-buffer-in-side-window buffer alist))))


;;
;; Commands

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
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (fboundp 'vc-refresh-state)
          (vc-refresh-state))
        (when (fboundp '+version-control|update-git-gutter)
          (+version-control|update-git-gutter))))))

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

;;;###autoload
(defun +magit*hub-settings--format-magithub.enabled ()
  "Change the setting to display 'false' as its default."
  (magit--format-popup-variable:choices "magithub.enabled" '("true" "false") "false"))

;;;###autoload
(defun +magit*hub-enabled-p ()
  "Disables magithub by default."
  (magithub-settings--value-or "magithub.enabled" nil
    #'magit-get-boolean))
