;;; lang/agda/config.el -*- lexical-binding: t; -*-

(defvar +agda-dir
  (when (executable-find "agda-mode")
    (file-name-directory (shell-command-to-string "agda-mode locate"))))

(def-package! agda-input
  :load-path +agda-dir)

(def-package! agda2-mode
  :mode "\\.agda\\'"
  :after agda-input
  :config
  (map! :map agda2-mode-map
                :localleader
                :n "l" #'agda2-load
                :n "d" #'agda2-infer-type-maybe-toplevel
                :n "o" #'agda2-module-contents-maybe-toplevel
                :n "n" #'agda2-compute-normalised-maybe-toplevel))
