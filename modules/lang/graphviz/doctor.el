;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/graphviz/doctor.el

(when (require 'graphviz-dot-mode nil t)
  (unless (executable-find graphviz-dot-dot-program)
    (warn! "Couldn't find dot. Previewing and exporting will not work")))
