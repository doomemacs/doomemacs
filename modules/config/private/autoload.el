;;; config/private/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +private/find-in-config ()
  "Open a file somewhere in `+private-config-path' via a fuzzy filename search."
  (interactive)
  (doom-project-find-file +private-config-path))

;;;###autoload
(defun +private/browse-config ()
  "Browse the files in `+private-config-path'."
  (interactive)
  (doom-project-browse +private-config-path))
