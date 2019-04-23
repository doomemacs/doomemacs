;;; core/autoload/cli.el -*- lexical-binding: t; -*-

(require 'core-cli)

(defun doom--run (command &optional yes)
  (let* ((default-directory doom-emacs-dir)
         (doom-auto-accept yes)
         (buf (get-buffer-create " *bin/doom*"))
         (wconf (current-window-configuration))
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
  (message (format! (green "Done!"))))


;;;###autoload
(defun doom//autoloads (&optional yes)
  "TODO"
  (interactive "P")
  (doom--run "autoloads" yes))

;;;###autoload
(defun doom//update (&optional yes)
  "TODO"
  (interactive "P")
  (doom--run "update" yes))

;;;###autoload
(defun doom//upgrade (&optional yes)
  "TODO"
  (interactive "P")
  (doom--run "upgrade" yes)
  (when (y-or-n-p "You must restart Emacs for the upgrade to take effect. Restart?")
    (doom/restart-and-restore)))

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
