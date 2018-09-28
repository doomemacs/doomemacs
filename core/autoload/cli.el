;;; core/autoload/cli.el -*- lexical-binding: t; -*-

(require 'core-cli)

(defun doom--run (command &optional yes)
  (let ((default-directory doom-emacs-dir)
        (doom-auto-accept yes))
    (let ((compilation-buffer-name-function (lambda (_) "*bin/doom*")))
      (compile (format "bin/doom %s" command) t))
    (while compilation-in-progress
      (sit-for 1))
    (when (y-or-n-p "Reload Doom config?")
      (doom/reload))
    (message "Done")))


;;;###autoload
(defun doom//update (&optional yes)
  "TODO"
  (interactive "P")
  (doom--run "update" yes))

;;;###autoload
(defun doom//upgrade (&optional yes)
  "TODO"
  (interactive "P")
  (doom--run "upgrade" yes))

;;;###autoload
(defun doom//install (&optional yes)
  "TODO"
  (interactive "P")
  (doom--run "install" yes))

;;;###autoload
(defun doom//autoremove (&optional yes)
  "TODO"
  (interactive "P")
  (doom--run "autoremove" yes))

;;;###autoload
(defun doom//refresh (&optional yes)
  "TODO"
  (interactive "P")
  (doom--run "refresh" yes))

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
  (run-hook-wrapped 'doom-reload-hook #'doom-try-run-hook)
  (message "Finished!"))
