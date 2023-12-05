;;; editor/parinfer/config.el -*- lexical-binding: t; -*-

(use-package! parinfer-rust-mode
  :when (bound-and-true-p module-file-suffix)
  :hook ((emacs-lisp-mode
          clojure-mode
          scheme-mode
          lisp-mode
          racket-mode
          fennel-mode
          hy-mode) . parinfer-rust-mode)
  :init
  (setq parinfer-rust-library
        (file-name-concat
         doom-data-dir "parinfer-rust/"
         (cond ((featurep :system 'macos)   "parinfer-rust-darwin.so")
               ((featurep :system 'linux)   "parinfer-rust-linux.so")
               ((featurep :system 'windows) "parinfer-rust-windows.dll")
               ((featurep :system 'bsd)     "libparinfer_rust.so")))
        parinfer-rust-auto-download (not (featurep :system 'bsd)))
  :config
  (map! :map parinfer-rust-mode-map
        :localleader
        "p" #'parinfer-rust-switch-mode
        "P" #'parinfer-rust-toggle-disable))
