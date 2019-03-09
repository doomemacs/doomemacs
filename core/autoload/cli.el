;;; core/autoload/cli.el -*- lexical-binding: t; -*-

(require 'core-cli)

(defun doom--run (command &optional yes)
  (let* ((default-directory doom-emacs-dir)
         (doom-auto-accept yes)
         (buf (get-buffer-create " *bin/doom*"))
         (wconf (current-window-configuration))
         (noninteractive t)
         (ignore-window-parameters t)
         (standard-output
          (lambda (char)
            (with-current-buffer buf
              (insert char)
              (when (memq char '(?\n ?\r))
                (ansi-color-apply-on-region (line-beginning-position -1) (line-end-position))
                (redisplay))))))
    (delete-other-windows)
    (switch-to-buffer buf)
    (redisplay)
    (cl-letf (((symbol-function 'message)
               (lambda (message &rest args)
                 (princ (apply #'format message args))
                 (terpri))))
      (doom-dispatch command nil))
    (print! (green "Done!"))
    (redisplay)
    (when (y-or-n-p "Return to your work?")
      (set-window-configuration wconf)
      (kill-buffer buf))))


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
