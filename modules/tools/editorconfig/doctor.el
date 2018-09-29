;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; tools/editorconfig/doctor.el

(unless (executable-find "editorconfig")
  (warn! "Couldn't find the editorconfig binary. Using native elisp version (slower)"))
