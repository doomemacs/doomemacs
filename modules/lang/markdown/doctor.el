;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;; lang/markdown/doctor.el

(when (featurep! +pandoc)
  (unless (executable-find "pandoc")
    (warn! "Couldn't find pandoc, markdown-mode may have issues")))
