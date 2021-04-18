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
        (concat user-emacs-directory
                ".local/etc/parinfer-rust/"
                (cond ((eq system-type 'darwin) "parinfer-rust-darwin.so")
                      ((eq system-type 'gnu/linux) "parinfer-rust-linux.so")
                      ((eq system-type 'windows-nt) "parinfer-rust-windows.dll")
                      ((eq system-type 'berkeley-unix) "libparinfer_rust.so"))))
  (setq parinfer-rust-auto-download (if (eq system-type 'berkeley-unix) nil t))
  :config
  (map! :map parinfer-rust-mode-map
        :localleader
        "m" #'parinfer-rust-switch-mode
        "M" #'parinfer-rust-toggle-disable))
