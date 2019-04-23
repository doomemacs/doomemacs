;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/markdown/doctor.el

(when (featurep! +pandoc)
  (unless (executable-find "pandoc")
    (warn! "Couldn't find pandoc, markdown-mode may have issues")))

(when (require 'markdown-mode nil t)
  (unless (executable-find markdown-command)
    (warn! "Couldn't find %S, can't export markdown to html"
           markdown-command)))
