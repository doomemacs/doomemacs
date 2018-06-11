;;; lang/emacs-lisp/+parinfer.el -*- lexical-binding: t; -*-


(def-package! parinfer
  :commands (parinfer-mode)
  :init
  (add-hook! (emacs-lisp-mode
              clojure-mode
              common-lisp-mode
              scheme-mode
              lisp-mode)
    (yas-minor-mode -1)
    (parinfer-mode))
  (setq parinfer-extensions
        '(defaults
           pretty-parens
           smart-tab
           smart-yank))
  (if (featurep! :feature evil)
      (push 'evil parinfer-extensions))
  :config
  (map! :map parinfer-mode-map
        :i "<tab>" #'parinfer-smart-tab:dwim-right
        :i "<backtab>" #'parinfer-smart-tab:dwim-left
        :localleader
        :nv "m" #'parinfer-toggle-mode))
