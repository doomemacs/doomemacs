;;; app/irc/autoload/settings.el -*- lexical-binding: t; -*-

;;;###autodef
(defun set-irc-server! (server letvars)
  "Registers an irc SERVER for circe.

See `circe-network-options' for details."
  (after! circe
    (push (cons server letvars) circe-network-options)))

;; FIXME obsolete :irc
;;;###autoload
(def-setting! :irc (server letvars)
  :obsolete set-irc-server!
  `(after! circe
     (push (cons ,server ,letvars) circe-network-options)))
