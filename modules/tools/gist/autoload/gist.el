;;; tools/gist/autoload/gist.el

;;;###autoload
(defun +gist/open-current ()
  (interactive)
  (gist-fetch-current)
  (doom/popup-close-all))

;;;###autoload
(defun +gist/kill-cache ()
  (interactive)
  (delete-directory (expand-file-name "gh" pcache-directory) t)
  (message "gist.el cache cleared"))
