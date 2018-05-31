;;; tools/magit/autoload.el -*- lexical-binding: t; -*-

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
