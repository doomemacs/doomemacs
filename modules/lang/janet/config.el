;;; lang/janet/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "project.janet"))

(use-package! janet-mode
  :hook (janet-mode . rainbow-delimiters-mode))

(use-package! inf-janet
  :init
  (after! janet-mode
    (set-repl-handler! 'janet-mode #'inf-janet)
    (add-hook 'janet-mode-hook #'inf-janet-minor-mode)
    (map! :map janet-mode-map
          :localleader
          "'" #'inf-janet
          "r" #'inf-janet-switch-to-repl
          (:prefix ("e" . "eval")
           "b" #'inf-janet-eval-buffer
           "d" #'inf-janet-eval-defun
           "e" #'inf-janet-eval-last-sexp
           "r" #'inf-janet-eval-region
           "s" #'inf-janet-eval-string)
          :map inf-janet-mode-map
          :localleader
          "c" #'inf-janet-clear-repl-buffer
          "e" #'inf-janet-eval-last-sexp)))
