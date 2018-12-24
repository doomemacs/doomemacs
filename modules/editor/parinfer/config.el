;;; editor/parinfer/config.el -*- lexical-binding: t; -*-

(def-package! parinfer
  :hook ((emacs-lisp-mode clojure-mode scheme-mode lisp-mode) . parinfer-mode)
  :init
  (setq parinfer-extensions
        '(defaults
          pretty-parens
          smart-tab
          smart-yank))
  (when (featurep! :feature evil +everywhere)
    (push 'evil parinfer-extensions))
  :config
  (map! :map parinfer-mode-map
        "\"" nil  ; smartparens handles this
        :i "<tab>"     #'parinfer-smart-tab:dwim-right-or-complete
        :i "<backtab>" #'parinfer-smart-tab:dwim-left
        :localleader
        :desc "Toggle parinfer-mode" "m" #'parinfer-toggle-mode))
