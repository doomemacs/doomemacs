;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/agda/doctor.el

(unless (executable-find "agda-mode")
  (warn! "Couldn't find agda-mode. Agda support won't work"))
