;;; lang/agda/config.el -*- lexical-binding: t; -*-

(defvar +agda-dir (string-remove-suffix "/agda2.el" (shell-command-to-string "agda-mode locate")))

(def-package! agda2
  :load-path +agda-dir)

