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

  (map! (:after ess-help
          :map ess-doc-map
          "h" #'ess-display-help-on-object
          "p" #'ess-R-dv-pprint
          "t" #'ess-R-dv-ctable
          [C-return] #'ess-eval-line
          [up]       #'comint-next-input
          [down]     #'comint-previous-input)

        :localleader
        :map ess-mode-map
        "," #'ess-eval-region-or-function-or-paragraph-and-step
        "'" #'R
        [tab]     #'ess-switch-to-inferior-or-script-buffer
        [backtab] #'ess-switch-process
        ;; REPL
        "B" #'ess-eval-buffer-and-go
        "b" #'ess-eval-buffer
        "d" #'ess-eval-region-or-line-and-step
        "D" #'ess-eval-function-or-paragraph-and-step
        "L" #'ess-eval-line-and-go
        "l" #'ess-eval-line
        "R" #'ess-eval-region-and-go
        "r" #'ess-eval-region
        "F" #'ess-eval-function-and-go
        "f" #'ess-eval-function
        ;; predefined keymaps
        "h" #'ess-doc-map
        "x" #'ess-extra-map
        "p" #'ess-r-package-dev-map
        "v" #'ess-dev-map
        ;; noweb
        :prefix "c"
        "C" #'ess-eval-chunk-and-go
        "c" #'ess-eval-chunk
        "d" #'ess-eval-chunk-and-step
        "m" #'ess-noweb-mark-chunk
        "p" #'ess-noweb-previous-chunk
        "n" #'ess-noweb-next-chunk))
