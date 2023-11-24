;;; lang/ess/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "DESCRIPTION"))


;;
;;; Packages

(use-package! ess
  :mode (("\\.sp\\'"           . S-mode)
         ("/R/.*\\.q\\'"       . R-mode)
         ("\\.[qsS]\\'"        . S-mode)
         ("\\.[Ss][s][c]\\'"   . S-mode)
         ("\\.SSC\\'"          . S-mode)
         ("\\.[rR]\\'"         . R-mode)
         ("\\.[rR]nw\\'"       . poly-noweb+r-mode)
         ("\\.[rR]md\\'"       . poly-markdown+r-mode)
         ("\\.[sS]nw\\'"       . poly-noweb+r-mode)
         ("\\.[rR]profile\\'"  . R-mode)
         ("NAMESPACE\\'"       . R-mode)
         ("CITATION\\'"        . R-mode)
         ("\\.omg\\'"          . omegahat-mode)
         ("\\.hat\\'"          . omegahat-mode)
         ("\\.lsp\\'"          . XLS-mode)
         ("\\.do\\'"           . STA-mode)
         ("\\.ado\\'"          . STA-mode)
         ("\\.[Ss][Aa][Ss]\\'" . SAS-mode)
         ("\\.[Ss]t\\'"        . S-transcript-mode)
         ("\\.Sout"            . S-transcript-mode)
         ("\\.[Rr]out"         . R-transcript-mode)
         ("\\.Rd\\'"           . Rd-mode)
         ("\\.[Bb][Uu][Gg]\\'" . ess-bugs-mode)
         ("\\.[Bb][Oo][Gg]\\'" . ess-bugs-mode)
         ("\\.[Bb][Mm][Dd]\\'" . ess-bugs-mode)
         ("\\.[Jj][Aa][Gg]\\'" . ess-jags-mode)
         ("\\.[Jj][Oo][Gg]\\'" . ess-jags-mode)
         ("\\.[Jj][Mm][Dd]\\'" . ess-jags-mode))
  :commands (R stata julia SAS ess-julia-mode)
  :init
  (unless (modulep! :lang julia)
    (add-to-list 'auto-mode-alist '("\\.[Jj][Ll]\\'" . ess-julia-mode)))
  :config
  (setq ess-offset-continued 'straight
        ess-use-flymake (or (not (modulep! :checkers syntax))
                            (modulep! :checkers syntax +flymake))
        ess-nuke-trailing-whitespace-p t
        ess-style 'DEFAULT
        ess-history-directory (expand-file-name "ess-history/" doom-cache-dir))

  (set-docsets! 'ess-r-mode "R")
  (when (modulep! +lsp)
    (add-hook 'ess-r-mode-local-vars-hook #'lsp! 'append))

  (when (modulep! +tree-sitter)
    (add-hook 'ess-r-mode-local-vars-hook #'tree-sitter! 'append))

  ;; WAIT lsp-mode over TRAMP needs manual configuration, and still it doesn't
  ;; work (see https://github.com/emacs-lsp/lsp-mode/issues/3579). In eglot
  ;; remote buffers should work out the box.
  ;;
  ;; The problem and the fix it is already known by the lsp-mode devs. So we
  ;; must be patient. The expected configuration should be something like this:

  (after! lsp-mode
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection '("R" "--slave" "-e" "languageserver::run()"))
      :major-modes '(ess-r-mode)
      :remote? t
      :server-id 'lsp-r-remote)))

  (set-repl-handler! 'ess-r-mode #'+ess/open-r-repl)
  (set-repl-handler! 'ess-julia-mode #'+ess/open-julia-repl)
  (set-lookup-handlers! '(ess-r-mode ess-julia-mode)
    :documentation #'ess-display-help-on-object)

  (set-evil-initial-state! 'ess-r-help-mode 'normal)
  (set-eval-handler! 'ess-help-mode #'ess-eval-region-and-go)
  (set-eval-handler! 'ess-r-help-mode #'ess-eval-region-and-go)

  (when (modulep! :completion company)
    (set-company-backend! '(ess-r-mode inferior-ess-r-mode)
      '(company-R-args company-R-objects
        company-R-library company-dabbrev-code :separate)))

  (setq-hook! 'ess-r-mode-hook
    ;; HACK Fix #2233: Doom continues comments on RET, but ess-r-mode doesn't
    ;;      have a sane `comment-line-break-function', so...
    comment-line-break-function nil)

  ;; HACK make the REPL buffer more responsive.
  (setq-hook! 'inferior-ess-mode-hook
    comint-scroll-to-bottom-on-input t
    comint-scroll-to-bottom-on-output t
    comint-move-point-for-output t)


  (map!
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
         "p"    #'ess-view-data-print
         [up]   #'comint-next-input
         [down] #'comint-previous-input
         [C-return] #'ess-eval-line)
   (:map ess-roxy-mode-map
    :i "RET" #'ess-indent-new-comment-line)
   (:map ess-mode-map
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
    "n" #'ess-noweb-next-chunk)))


(use-package! stan-mode
  :when (modulep! +stan)
  :hook (stan-mode . stan-mode-setup)
  :hook (stan-mode . eldoc-stan-setup)
  :init
  (use-package! company-stan
    :when (modulep! :completion company)
    :hook (stan-mode . company-stan-setup))

  (use-package! flycheck-stan
    :when (modulep! :checkers syntax -flymake)
    :hook (stan-mode . flycheck-stan-stanc2-setup)
    :hook (stan-mode . flycheck-stan-stanc3-setup)))

(use-package!  quarto-mode
  :mode (("\\.[qQ][mM][dD]" . poly-quarto-mode)))
