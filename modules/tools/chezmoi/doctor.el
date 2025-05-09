;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; tools/chezmoi/doctor.el

(unless (executable-find "chezmoi")
  (warn! "Couldn't find the chezmoi binary."))
