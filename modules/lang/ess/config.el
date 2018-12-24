;;; lang/ess/config.el -*- lexical-binding: t; -*-

(def-package! ess
  :commands (stata SAS)
  :init
  (setq ess-smart-S-assign-key nil)
  (unless (featurep! :lang julia)
    (add-to-list 'auto-mode-alist '("\\.jl\\'" . ess-julia-mode)))
  :config
  (setq ess-offset-continued 'straight
        ess-expression-offset 2
        ess-nuke-trailing-whitespace-p t
        ess-default-style 'DEFAULT)

  (add-hook 'ess-mode-hook #'display-line-numbers-mode)

  (set-repl-handler! 'ess-mode #'+ess/r-repl)
  (set-lookup-handlers! 'ess-mode :documentation #'ess-display-help-on-object)

  (after! ess-help
    (define-key! ess-doc-map
      "h" #'ess-display-help-on-object
      "p" #'ess-R-dv-pprint
      "t" #'ess-R-dv-ctable)
    (define-key! ess-doc-map
      [s-return] #'ess-eval-line
      [up]       #'comint-next-input
      [down]     #'comint-previous-input))
  (map! :map ess-mode-map
        :localleader
        :nv ","        #'ess-eval-region-or-function-or-paragraph-and-step
        :n "'"         #'R
        :n "<tab>"     #'ess-switch-to-inferior-or-script-buffer
        :n "<backtab>" #'ess-switch-process
        :n ;; REPL
        :n "B"         #'ess-eval-buffer-and-go
        :n "b"         #'ess-eval-buffer
        :nv "d"        #'ess-eval-region-or-line-and-step
        :n "D"         #'ess-eval-function-or-paragraph-and-step
        :n "L"         #'ess-eval-line-and-go
        :n "l"         #'ess-eval-line
        :nv "R"        #'ess-eval-region-and-go
        :nv "r"        #'ess-eval-region
        :n "F"         #'ess-eval-function-and-go
        :n "f"         #'ess-eval-function
        ;; predefined keymaps
        :n "h"         #'ess-doc-map
        :n "x"         #'ess-extra-map
        :n "p"         #'ess-r-package-dev-map
        :n "v"         #'ess-dev-map
        ;; noweb
        :n "cC"        #'ess-eval-chunk-and-go
        :n "cc"        #'ess-eval-chunk
        :n "cd"        #'ess-eval-chunk-and-step
        :n "cm"        #'ess-noweb-mark-chunk
        :n "cp"        #'ess-noweb-previous-chunk
        :n "cn"        #'ess-noweb-next-chunk))
