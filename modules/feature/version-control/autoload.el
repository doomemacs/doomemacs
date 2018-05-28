;;; feature/version-control/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +vcs-root ()
  "Return the root git repo URL for the current file."
  (let* ((remote (git-link--select-remote))
         (remote-url (git-link--remote-url remote))
         (remote-info (if remote-url (git-link--parse-remote remote-url))))
    (if remote-info
        (format "https://%s/%s" (car remote-info) (cadr remote-info))
      (error  "Remote `%s' is unknown or contains an unsupported URL" remote))))

(defvar git-link-open-in-browser)
;;;###autoload
(defun +vcs/git-browse ()
  "Open the website for the current version controlled file. Fallback to
repository root."
  (interactive)
  (require 'git-link)
  (cl-destructuring-bind (beg end)
      (if buffer-file-name (git-link--get-region))
    (let ((git-link-open-in-browser t))
      (git-link (git-link--select-remote) beg end))))

;;;###autoload
(defun +vcs/git-browse-issues ()
  "Open the issues page for current repo."
  (interactive)
  (browse-url (format "%s/issues" (+vcs-root))))

;;;###autoload
(defun +vcs/git-browse-pulls ()
  "Open the pull requests page for current repo."
  (interactive)
  (browse-url (format "%s/pulls" (+vcs-root))))

;;;###autoload
(defun +vcs*update-header-line (revision)
  "Show revision details in the header-line, instead of the minibuffer.

Sometimes I forget `git-timemachine' is enabled in a buffer. Putting revision
info in the `header-line-format' is a good indication."
  (let* ((date-relative (nth 3 revision))
         (date-full (nth 4 revision))
         (author (if git-timemachine-show-author (concat (nth 6 revision) ": ") ""))
         (sha-or-subject (if (eq git-timemachine-minibuffer-detail 'commit) (car revision) (nth 5 revision))))
    (setq header-line-format
          (format "%s%s [%s (%s)]"
                  (propertize author 'face 'git-timemachine-minibuffer-author-face)
                  (propertize sha-or-subject 'face 'git-timemachine-minibuffer-detail-face)
                  date-full date-relative))))

;;;###autoload
(defun +vcs|enable-smerge-mode-maybe ()
  "Auto-enable `smerge-mode' when merge conflict is detected."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil :noerror)
      (smerge-mode 1)
      (when (and (featurep 'hydra)
                 +vcs-auto-hydra-smerge)
        (+hydra-smerge/body)))))
