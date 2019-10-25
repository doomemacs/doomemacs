;;; emacs/vc/autoload/vc.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +vc-git-root-url ()
  "Return the root git repo URL for the current file."
  (require 'git-link)
  (let* ((remote (git-link--select-remote))
         (remote-url (git-link--remote-url remote))
         (remote-info (if remote-url (git-link--parse-remote remote-url))))
    (if remote-info
        (format "https://%s/%s" (car remote-info) (cadr remote-info))
      (error  "Remote `%s' is unknown or contains an unsupported URL" remote))))

(defvar git-link-open-in-browser)
(defvar git-link-use-commit)
;;;###autoload
(defun +vc/git-browse-region-or-line (&optional arg)
  "Open the website for the current line of this version controlled file.
Uses the currently checked out branch. If prefix ARG, then use 'master' branch.
If an url can't be ascertained, opens the repository's root."
  (interactive "P")
  (require 'git-link)
  (let ((git-link-default-branch (if arg "master" git-link-default-branch))
        current-prefix-arg ; don't propagate to `git-link'
        git-link-use-commit)
    (cl-destructuring-bind (beg end)
        (if buffer-file-name (git-link--get-region))
      (let ((git-link-open-in-browser t))
        (git-link (git-link--select-remote) beg end)))))

;;;###autoload
(defun +vc-update-header-line-a (revision)
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
