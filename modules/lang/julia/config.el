;;; lang/julia/config.el -*- lexical-binding: t; -*-

(use-package! julia-mode
  :interpreter "julia"
  :config

  (when (featurep! +lsp)
    (add-hook 'julia-mode-hook #'lsp!)
    (setq lsp-julia-default-environment "~/.julia/environments/v1.0")
    )

  (set-repl-handler! 'julia-mode #'+julia/open-repl)
  (add-hook 'julia-mode-hook #'julia-repl-mode)

  ;; Borrow matlab.el's fontification of math operators
  ;; From <https://ogbe.net/emacsconfig.html>
  (dolist (mode '(julia-mode ess-julia-mode))
    (font-lock-add-keywords
     mode
     `((,(let ((OR "\\|"))
           (concat "\\(" ;; stolen `matlab.el' operators first
                   "[<>!]=?" OR
                   "\\.[/*^']" OR
                   "==" OR
                   "=>" OR
                   "\\<xor\\>" OR
                   "[-+*\\/^&|$]=?" OR ;; this has to come before next (updating operators)
                   "[-!^&|*+\\/~:]" OR
                   ;; more extra julia operators follow
                   "[%$]" OR
                   ;; bitwise operators
                   ">>>" OR ">>" OR "<<" OR
                   ">>>=" OR ">>" OR "<<" OR
                   ;; comparison
                   "[<>!]=?" OR
                   "\\)"))
        1 font-lock-type-face)))))


(after! julia-repl
  (add-hook 'julia-repl-hook #'julia-repl-use-emacsclient)

  (defadvice! julia-repl--buffer-name (&optional executable-key suffix)
    :override #'julia-repl--inferior-buffer-name
    "Name for a Julia REPL inferior buffer. Uses workspace name for doom emacs"
    (concat julia-repl-inferior-buffer-name-base ":" (+workspace-current-name)))

  (set-popup-rule! "^\\*julia.*\\*$" :ttl nil)

  (defadvice! after-julia-repl-advice (inferior-buffer)
    :after #'julia-repl--setup-term
    (with-current-buffer inferior-buffer
      (term-set-escape-char ?\C-c)      ; override default of C-x..
      ))
  )

(use-package! lsp-julia
  :when (featurep! +lsp)
  :after lsp-clients
  )
