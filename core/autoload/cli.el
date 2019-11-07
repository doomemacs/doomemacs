;;; core/autoload/cli.el -*- lexical-binding: t; -*-

;; Externs
(defvar evil-collection-mode-list)

;;;###autoload
(defun doom--cli-run (command &rest _args)
  (when (featurep 'general)
    (general-auto-unbind-keys))
  (let* ((evil-collection-mode-list nil)
         (default-directory doom-emacs-dir)
         (buf (get-buffer-create " *bin/doom*"))
         (doom-format-backend 'ansi)
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
    (doom--cli-run "autoloads")))

;;;###autoload
(defun doom//update (&optional yes)
  "TODO"
  (interactive "P")
  (let ((doom-auto-accept yes))
    (doom--cli-run "update")))

;;;###autoload
(defun doom//upgrade (&optional yes)
  "TODO"
  (interactive "P")
  (let ((doom-auto-accept yes))
    (doom--cli-run "upgrade"))
  (when (y-or-n-p "You must restart Emacs for the upgrade to take effect. Restart?")
    (doom/restart-and-restore)))

;;;###autoload
(defun doom//install (&optional yes)
  "TODO"
  (interactive "P")
  (let ((doom-auto-accept yes))
    (doom--cli-run "install")))

;;;###autoload
(defun doom//autoremove (&optional yes)
  "TODO"
  (interactive "P")
  (let ((doom-auto-accept yes))
    (doom--cli-run "autoremove")))

;;;###autoload
(defun doom//refresh (&optional yes)
  "TODO"
  (interactive "P")
  (let ((doom-auto-accept yes))
    (doom--cli-run "refresh")))



;;
;;; Library

;;;###autoload
(defun doom-call-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Returns (STATUS . OUTPUT) when it is done, where STATUS is the returned error
code of the process and OUTPUT is its stdout output."
  (with-temp-buffer
    (cons (or (apply #'call-process command nil t nil args)
              -1)
          (string-trim (buffer-string)))))

;;;###autoload
(defun doom-exec-process (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Unlike `doom-call-process', this pipes output to `standard-output' on the fly to
simulate 'exec' in the shell, so batch scripts could run external programs
synchronously without sacrificing their output.

Warning: freezes indefinitely on any stdin prompt."
  ;; FIXME Is there any way to handle prompts?
  (with-temp-buffer
    (cons (let ((process
                 (make-process :name "doom-sh"
                               :buffer (current-buffer)
                               :command (cons command args)
                               :connection-type 'pipe))
                done-p)
            (set-process-filter
             process (lambda (process output)
                       (princ output (current-buffer))
                       (princ output)))
            (set-process-sentinel
             process (lambda (process _event)
                       (when (memq (process-status process) '(exit stop))
                         (setq done-p t))))
            (while (not done-p)
              (sit-for 0.1))
            (process-exit-status process))
          (string-trim (buffer-string)))))
