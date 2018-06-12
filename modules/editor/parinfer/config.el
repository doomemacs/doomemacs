;;; editor/parinfer/config.el -*- lexical-binding: t; -*-

(def-package! parinfer
  :hook ((emacs-lisp clojure-mode common-lisp scheme lisp) . parinfer-mode)
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
        :i "<tab>" #'parinfer-smart-tab:dwim-right-or-complete
        :i "<backtab>" #'parinfer-smart-tab:dwim-left
        :localleader
        :nv "m" #'parinfer-toggle-mode))
