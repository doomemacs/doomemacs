;;; lang/agda/config.el -*- lexical-binding: t; -*-

(defvar +agda-dir
  (when (executable-find "agda-mode")
    (file-name-directory (shell-command-to-string "agda-mode locate")))
  "TODO")


(def-package! agda2
  :load-path +agda-dir
  :defer t)
