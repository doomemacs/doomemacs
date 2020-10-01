;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; os/exwm/doctor.el

(unless (executable-find "firefox")
  (warn! "Couldn't find firefox. exwm-firefox will not work"))
