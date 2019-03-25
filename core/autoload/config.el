;;; core/autoload/config.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom/open-private-config ()
  "TODO"
  (interactive)
  (unless (file-directory-p doom-private-dir)
    (make-directory doom-private-dir t))
  (doom-project-browse doom-private-dir))

;;;###autoload
(defun doom/find-file-in-private-config ()
  "TODO"
  (interactive)
  (doom-project-find-file doom-private-dir))

;;;###autoload
(defun doom/reload (&optional force-p)
  "Reloads your config. This is experimental!

If called from a noninteractive session, this will try to communicate with a
live server (if one is found) to tell it to run this function.

If called from an interactive session, tries to reload autoloads files (if
necessary), reinistalize doom (via `doom-initialize') and reloads your private
init.el and config.el. Then runs `doom-reload-hook'."
  (interactive "P")
  (require 'core-cli)
  (doom-reload-autoloads force-p)
  (setq load-path doom-site-load-path)
  (let (doom-init-p)
    (doom-initialize))
  (with-demoted-errors "PRIVATE CONFIG ERROR: %s"
    (doom-initialize-modules 'force))
  (when (bound-and-true-p doom-packages)
    (doom/reload-packages))
  (run-hook-wrapped 'doom-reload-hook #'doom-try-run-hook)
  (message "Finished!"))
