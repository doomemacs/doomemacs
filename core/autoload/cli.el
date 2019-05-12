;;; core/autoload/cli.el -*- lexical-binding: t; -*-

(require 'core-cli)

;;;###autoload
(defun doom-cli-run (command &rest _args)
  (when (featurep 'general)
    (general-auto-unbind-keys))
  (let* ((evil-collection-mode-list nil)
         (default-directory doom-emacs-dir)
         (buf (get-buffer-create " *bin/doom*"))
         (doom-message-backend 'ansi)
         (ignore-window-parameters t)
         (noninteractive t)
         (standard-output
          (lambda (char)
            (with-current-buffer buf
              (insert char)
              (when (memq char '(?\n ?\r))
                (ansi-color-apply-on-region (line-beginning-position -1) (line-end-position))
                (redisplay))))))
    (doom-initialize t)
    (setq doom-modules (doom-modules))
    (doom-initialize-modules t)
    (doom-initialize-packages t)
    (with-current-buffer (switch-to-buffer buf)
      (erase-buffer)
      (require 'package)
      (redisplay)
      (doom-dispatch command nil)
      (print! (green "\nDone!"))))
  (when (featurep 'general)
    (general-auto-unbind-keys 'undo))
  (message (format! (green "Done!"))))


;;;###autoload
(defun doom//autoloads (&optional yes)
  "TODO"
  (interactive "P")
  (let ((doom-auto-accept yes))
    (doom-cli-run "autoloads")))

;;;###autoload
(defun doom//update (&optional yes)
  "TODO"
  (interactive "P")
  (let ((doom-auto-accept yes))
    (doom-cli-run "update")))

;;;###autoload
(defun doom//upgrade (&optional yes)
  "TODO"
  (interactive "P")
  (let ((doom-auto-accept yes))
    (doom-cli-run "upgrade"))
  (when (y-or-n-p "You must restart Emacs for the upgrade to take effect. Restart?")
    (doom/restart-and-restore)))

;;;###autoload
(defun doom//install (&optional yes)
  "TODO"
  (interactive "P")
  (let ((doom-auto-accept yes))
    (doom-cli-run "install")))

;;;###autoload
(defun doom//autoremove (&optional yes)
  "TODO"
  (interactive "P")
  (let ((doom-auto-accept yes))
    (doom-cli-run "autoremove")))

;;;###autoload
(defun doom//refresh (&optional yes)
  "TODO"
  (interactive "P")
  (let ((doom-auto-accept yes))
    (doom-cli-run "refresh")))
