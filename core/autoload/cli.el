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
