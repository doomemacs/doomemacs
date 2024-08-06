;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/graphviz/doctor.el

(when (require 'graphviz-dot-mode nil t)
  ;; graphviz
  (unless (executable-find "dot")
    (warn! "Couldn't find dot. Previewing and exporting will not work")))
