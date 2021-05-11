;;; lang/julia/config.el -*- lexical-binding: t; -*-

(use-package! julia-mode
  :interpreter "julia"
  :config
  (set-repl-handler! 'julia-mode #'+julia/open-repl)

  ;; Borrow matlab.el's fontification of math operators. From
  ;; <https://web.archive.org/web/20170326183805/https://ogbe.net/emacsconfig.html>
  (dolist (mode '(julia-mode ess-julia-mode))
    (font-lock-add-keywords
     mode
     `((,(let ((OR "\\|"))
           (concat "\\("  ; stolen `matlab.el' operators first
                   ;; `:` defines a symbol in Julia and must not be highlighted
                   ;; as an operator. The only operators that start with `:` are
                   ;; `:<` and `::`. This must be defined before `<`.
                   "[:<]:" OR
                   "[<>]=?" OR
                   "\\.[/*^']" OR
                   "===" OR
                   "==" OR
                   "=>" OR
                   "\\<xor\\>" OR
                   "[-+*\\/^&|$]=?" OR  ; this has to come before next (updating operators)
                   "[-^&|*+\\/~]" OR
                   ;; Julia variables and names can have `!`. Thus, `!` must be
                   ;; highlighted as a single operator only in some
                   ;; circumstances. However, full support can only be
                   ;; implemented by a full parser. Thus, here, we will handle
                   ;; only the simple cases.
                   "[[:space:]]!=?=?" OR "^!=?=?" OR
                   ;; The other math operators that starts with `!`.
                   ;; more extra julia operators follow
                   "[%$]" OR
                   ;; bitwise operators
                   ">>>" OR ">>" OR "<<" OR
                   ">>>=" OR ">>" OR "<<" OR
                   "\\)"))
        1 font-lock-type-face)))))


(use-package! julia-repl
  :preface (defvar +julia-repl-start-hook nil)
  :hook (julia-mode . julia-repl-mode)
  :hook (+julia-repl-start . +julia-override-repl-escape-char-h)
  :hook (+julia-repl-start . julia-repl-use-emacsclient)
  :config
  (set-popup-rule! "^\\*julia.*\\*$" :ttl nil)

  (when (featurep! :ui workspaces)
    (defadvice! +julia--namespace-repl-buffer-to-workspace-a (&optional executable-key suffix)
      "Name for a Julia REPL inferior buffer. Uses workspace name for doom emacs"
      :override #'julia-repl--inferior-buffer-name
      (concat julia-repl-inferior-buffer-name-base ":" (+workspace-current-name))))

  (defadvice! +julia--run-start-hook-a (inferior-buffer)
    "Run `+julia-repl-start-hook' before displaying the REPL."
    :after #'julia-repl--setup-term
    (with-current-buffer inferior-buffer
      (run-hooks '+julia-repl-start-hook)))

  (defun +julia-override-repl-escape-char-h ()
    "Use C-c instead of C-x for escaping."
    (term-set-escape-char ?\C-c)))


(when (featurep! +lsp)
  (add-hook 'julia-mode-local-vars-hook #'lsp!))


(use-package! lsp-julia
  :when (featurep! +lsp)
  :unless (featurep! :tools lsp +eglot)
  :after lsp-mode
  :preface (setq lsp-julia-default-environment nil)
  :init
  ;; If no environment is set, then auto-detect one in ~/.julia/environments/,
  ;; falling back to `lsp-julia-default-environment's default.
  (unless lsp-julia-default-environment
    (setq lsp-julia-default-environment
          (or (car (last (doom-glob "~/.julia/environments/v*")))
              "~/.julia/environments/v1.0")))
  :config
  ;; See non-Jedi/lsp-julia#35
  (setq-hook! 'julia-mode-hook
    lsp-enable-folding t
    lsp-folding-range-limit 100))


(use-package! eglot-jl
  :when (featurep! +lsp)
  :when (featurep! :tools lsp +eglot)
  :after eglot
  :preface
  ;; Prevent auto-install of LanguageServer.jl
  (setq eglot-jl-language-server-project "~/.julia/environments/v1.0")
  :init
  ;; Prevent timeout while installing LanguageServer.jl
  (setq-hook! 'julia-mode-hook eglot-connect-timeout (max eglot-connect-timeout 60))
  :config (eglot-jl-init))
