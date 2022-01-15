;;; editor/parinfer/config.el -*- lexical-binding: t; -*-

(use-package! parinfer-rust-mode
  :when (bound-and-true-p module-file-suffix)
  :hook ((emacs-lisp-mode
          clojure-mode
          scheme-mode
          lisp-mode
          racket-mode
          hy-mode) . parinfer-rust-mode)
  :init
  (setq parinfer-rust-library
        (concat doom-etc-dir "parinfer-rust/"
                (cond (IS-MAC "parinfer-rust-darwin.so")
                      (IS-LINUX "parinfer-rust-linux.so")
                      (IS-WINDOWS "parinfer-rust-windows.dll")
                      (IS-BSD "libparinfer_rust.so")))
        parinfer-rust-auto-download (not IS-BSD))
  :config
  (map! :map parinfer-rust-mode-map
        :localleader
        "p" #'parinfer-rust-switch-mode
        "P" #'parinfer-rust-toggle-disable))
