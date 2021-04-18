;;; app/irc/autoload/settings.el -*- lexical-binding: t; -*-

;;;###autodef
(defun set-irc-server! (server plist)
  "Registers an irc SERVER for circe.

SERVER can either be a name for the network (in which case you must specify a
:host), or it may be the hostname itself, in which case it will be used as the
:host.

See `circe-network-options' for details."
  (after! circe
    (unless (plist-member plist :host)
      (plist-put! plist :host server))
    (setf (alist-get server circe-network-options
                     nil nil #'equal)
          plist)))
