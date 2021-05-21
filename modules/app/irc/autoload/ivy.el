;;; app/irc/autoload/ivy.el -*- lexical-binding: t; -*-
;;;###if (featurep! :completion ivy)

;;;###autoload
(defun +irc/ivy-jump-to-channel (&optional this-server)
  "Jump to an open channel or server buffer with ivy. If THIS-SERVER (universal
argument) is non-nil only show channels in current server."
  (interactive "P")
  (if (not (circe-server-buffers))
      (message "No circe buffers available")
    (when (and this-server (not circe-server-buffer))
      (setq this-server nil))
    (ivy-read (format "Jump to%s: " (if this-server (format " (%s)" (buffer-name circe-server-buffer)) ""))
              (cl-loop with servers = (if this-server (list circe-server-buffer) (circe-server-buffers))
                       with current-buffer = (current-buffer)
                       for server in servers
                       collect (buffer-name server)
                       nconc
                       (with-current-buffer server
                         (cl-loop for buf in (circe-server-chat-buffers)
                                  unless (eq buf current-buffer)
                                  collect (format "  %s" (buffer-name buf)))))
              :action #'+irc--ivy-switch-to-buffer-action
              :preselect (buffer-name (current-buffer))
              :keymap ivy-switch-buffer-map
              :caller '+irc/ivy-jump-to-channel)))

(defun +irc--ivy-switch-to-buffer-action (buffer)
  (when (stringp buffer)
    (ivy--switch-buffer-action (string-trim-left buffer))))
