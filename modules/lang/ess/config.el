;;; lang/ess/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "DESCRIPTION"))


;;
;;; Packages

(use-package! ess
  :commands stata SAS
  :init
  (unless (featurep! :lang julia)
    (add-to-list 'auto-mode-alist '("\\.jl\\'" . ess-julia-mode)))
  :config
  (setq ess-offset-continued 'straight
        ess-use-flymake (not (featurep! :checkers syntax))
        ess-nuke-trailing-whitespace-p t
        ess-style 'DEFAULT
        ess-history-directory (expand-file-name "ess-history/" doom-cache-dir))

  (set-docsets! 'ess-r-mode "R")
  (when (featurep! +lsp)
    (add-hook 'ess-r-mode-local-vars-hook #'lsp!))

  (set-repl-handler! 'ess-r-mode #'+ess/open-r-repl)
  (set-repl-handler! 'ess-julia-mode #'+ess/open-julia-repl)
  (set-lookup-handlers! '(ess-r-mode ess-julia-mode)
    :documentation #'ess-display-help-on-object)

  (set-evil-initial-state! 'ess-r-help-mode 'normal)
  (set-eval-handler! 'ess-help-mode #'ess-eval-region-and-go)
  (set-eval-handler! 'ess-r-help-mode #'ess-eval-region-and-go)

  (set-company-backend! 'ess-r-mode
    '(company-R-args company-R-objects company-dabbrev-code :separate))

  (setq-hook! 'ess-r-mode-hook
    ;; HACK Fix #2233: Doom continues comments on RET, but ess-r-mode doesn't
    ;;      have a sane `comment-line-break-function', so...
    comment-line-break-function nil)

  (map! (:after ess-help
          (:map ess-help-mode-map
            :n "q"  #'kill-current-buffer
            :n "Q"  #'ess-kill-buffer-and-go
            :n "K"  #'ess-display-help-on-object
            :n "go" #'ess-display-help-in-browser
            :n "gO" #'ess-display-help-apropos
            :n "gv" #'ess-display-vignettes
            :m "]]" #'ess-skip-to-next-section
            :m "[[" #'ess-skip-to-previous-section)
          (:map ess-doc-map
            "h"    #'ess-display-help-on-object
            "p"    #'ess-R-dv-pprint
            "t"    #'ess-R-dv-ctable
            [up]   #'comint-next-input
            [down] #'comint-previous-input
            [C-return] #'ess-eval-line))

        :map ess-mode-map
        :n [C-return] #'ess-eval-line
        :localleader
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
        "h" 'ess-doc-map
        "x" 'ess-extra-map
        "p" 'ess-r-package-dev-map
        "v" 'ess-dev-map
        ;; noweb
        :prefix "c"
        "C" #'ess-eval-chunk-and-go
        "c" #'ess-eval-chunk
        "d" #'ess-eval-chunk-and-step
        "m" #'ess-noweb-mark-chunk
        "p" #'ess-noweb-previous-chunk
        "n" #'ess-noweb-next-chunk))
