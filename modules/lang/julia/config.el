;;; lang/julia/config.el -*- lexical-binding: t; -*-

(use-package! julia-mode
  :interpreter "julia"
  :config
  (set-repl-handler! 'julia-mode #'+julia/open-repl)

  ;; Borrow matlab.el's fontification of math operators. From
  ;; <https://ogbe.net/emacsconfig.html>
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


(use-package! lsp-julia
  :when (and (featurep! +lsp) (not (featurep! :tools lsp +eglot)))
  :after lsp-mode
  :preface
  (setq lsp-julia-default-environment "~/.julia/environments/v1.0")
  (when (featurep! +lsp)
    (add-hook 'julia-mode-local-vars-hook #'lsp!)
    (setq lsp-julia-package-dir nil)))

(use-package! eglot-jl
  :when (and (featurep! +lsp) (featurep! :tools lsp +eglot))
  :after eglot
  :preface
  ;; Prevent auto-install of LanguageServer.jl
  (setq eglot-jl-language-server-project "~/.julia/environments/v1.0")
  ;; Prevent timeout while installing LanguageServer.jl
  (add-hook! 'julia-mode-hook
    (defun +julia-eglot-timeout-h ()
      "Ensure that the `eglot-connect-timeout' is at least 60s in `julia-mode' buffers."
      (when (< eglot-connect-timeout 60)
        (setq-local eglot-connect-timeout 60))))
  :config
  (eglot-jl-init))
