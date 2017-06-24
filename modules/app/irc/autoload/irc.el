;;; app/irc/autoload/email.el -*- lexical-binding: t; -*-

(defvar +irc--workspace-name "*IRC*")

(defun +irc-setup-wconf (&optional inhibit-workspace)
  (unless inhibit-workspace
    (+workspace-switch +irc--workspace-name t))
  (let ((buffers (doom-buffers-in-mode 'circe-mode nil t)))
    (if buffers
        (ignore (switch-to-buffer (car buffers)))
      (require 'circe)
      (delete-other-windows)
      (switch-to-buffer (doom-fallback-buffer))
      t)))

;;;###autoload
(defun =irc (&optional inhibit-workspace)
  "Connect to IRC and auto-connect to all registered networks.

If INHIBIT-WORKSPACE (the universal argument) is non-nil, don't spawn a new
workspace for it."
  (interactive "P")
  (and (+irc-setup-wconf inhibit-workspace)
       (cl-loop for network in circe-network-options
                collect (circe (car network)))))

;;;###autoload
(defun +irc/connect (&optional inhibit-workspace)
  "Connect to a specific registered server.

If INHIBIT-WORKSPACE (the universal argument) is non-nil, don't spawn a new
workspace for it."
  (interactive "P")
  (and (+irc-setup-wconf inhibit-workspace)
       (call-interactively #'circe)))

;;;###autoload
(defun +irc/quit ()
  "Kill current circe session and workgroup."
  (interactive)
  (if (y-or-n-p "Really kill IRC session?")
      (let (circe-channel-killed-confirmation
            circe-server-killed-confirmation)
        (when +irc--defer-timer
          (cancel-timer +irc--defer-timer))
        (disable-circe-notifications)
        (mapc #'kill-buffer (doom-buffers-in-mode 'circe-mode (buffer-list) t))
        (when (equal (+workspace-current-name) +irc--workspace-name)
          (+workspace/delete +irc--workspace-name)))
    (message "Aborted")))

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
