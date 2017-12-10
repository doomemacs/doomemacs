;;; tools/gist/autoload/gist.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +gist/open-current ()
  (interactive)
  (gist-fetch-current)
  (when-let* ((win (get-buffer-window "*github:gists*")))
    (doom/popup-close win)))

;;;###autoload
(defun +gist/kill-cache ()
  "Clears the gist cache. Necessary when a faulty cache causes gist.el to be
entirely unuseable."
  (interactive)
  (delete-directory (expand-file-name "gh" pcache-directory) t)
  (message "gist.el cache cleared"))
