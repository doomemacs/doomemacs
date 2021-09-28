;;; editor/parinfer/config.el -*- lexical-binding: t; -*-

(use-package! parinfer
  :unless (featurep! +rust)
  :hook ((emacs-lisp-mode
          clojure-mode
          scheme-mode
          lisp-mode
          racket-mode
          hy-mode) . parinfer-mode)
  :init
  (setq parinfer-extensions
        '(defaults
          pretty-parens
          smart-tab
          smart-yank))
  (when (featurep! :editor evil +everywhere)
    (push 'evil parinfer-extensions))
  :config
  (map! :map parinfer-mode-map
        "\"" nil  ; smartparens handles this
        :i "<tab>"     #'parinfer-smart-tab:dwim-right-or-complete
        :i "<backtab>" #'parinfer-smart-tab:dwim-left
        :localleader
        "m" #'parinfer-toggle-mode))


(use-package! parinfer-rust-mode
  :when (featurep! +rust)
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
        "m" #'parinfer-rust-switch-mode
        "M" #'parinfer-rust-toggle-disable))
