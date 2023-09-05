;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; app/irc/doctor.el

(when (memq 'circe-notifications doom-disabled-packages)
  (warn! "Circe Notifications has been disabled, You will not receive desktop notifications from IRC channels."))
