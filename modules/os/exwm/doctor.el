;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; os/exwm/doctor.el

(when (featurep! +firefox)
  (unless (executable-find "firefox")
    (warn! "Couldn't find firefox. exwm-firefox will not work")))
