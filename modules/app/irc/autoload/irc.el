;;; app/irc/autoload/email.el -*- lexical-binding: t; -*-

;;;###autoload
(defun =irc ()
  "Connect to IRC."
  (interactive)
  (call-interactively #'circe))

;;;###autoload
(defun +irc/connect-all ()
  "Connect to all `:irc' defined servers."
  (interactive)
  ;; force a library load for +irc--accounts
  (circe--version)
  (cl-loop for network in +irc--accounts
           collect (circe (car network))))

