;;; feature/version-control/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +vcs-root ()
  "Get git url root."
  (let ((remote (git-link--select-remote)))
    (if (git-link--remote-host remote)
        (format "https://%s/%s"
                (git-link--remote-host remote)
                (git-link--remote-dir remote))
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
  "Open the github issues page for current repo."
  (interactive)
  (if-let (root (+vcs-root))
      (browse-url (concat root "/issues"))
    (user-error "No git root found!")))

;;;###autoload
(defun +vcs|init-header-line ()
  "Toggle the git-timemachine header-line on activate. Use this on
`git-timemachine-mode-hook'."
  (if git-timemachine-mode
      (+vcs*update-header-line)
    (setq-local header-line-format nil)))

;;;###autoload
(defun +vcs|enable-smerge-mode-maybe ()
  "Auto-enable `smerge-mode' when merge conflict is detected."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil :noerror)
      (smerge-mode 1)
      (when +vcs-auto-hydra-smerge (+hydra-smerge/body)))))

;;;###autoload
(defun +vcs*update-header-line (&rest _)
  "Show revision details in the header-line, instead of the minibuffer.

Sometimes I forget `git-timemachine' is enabled in a buffer. Putting info into,
putting them in `header-line-format' has better visibility."
  (when (and git-timemachine-mode git-timemachine-revision)
    (let* ((revision git-timemachine-revision)
           (date-relative (nth 3 revision))
           (date-full (nth 4 revision))
           (author (if git-timemachine-show-author (concat (nth 6 revision) ": ") ""))
           (sha-or-subject (if (eq git-timemachine-minibuffer-detail 'commit) (car revision) (nth 5 revision))))
      (setq-local
       header-line-format
       (format "%s%s [%s (%s)]"
               (propertize author 'face 'git-timemachine-minibuffer-author-face)
               (propertize sha-or-subject 'face 'git-timemachine-minibuffer-detail-face)
               date-full date-relative)))))
