;;; lang/julia/config.el -*- lexical-binding: t; -*-

(use-package! julia-mode
  :interpreter "julia"
  :config
  (unless (modulep! +snail)
    (set-repl-handler! 'julia-mode #'+julia/open-repl))

  (when (modulep! +lsp)
    (add-hook 'julia-mode-local-vars-hook #'lsp! 'append))

  (when (modulep! +tree-sitter)
    (add-hook 'julia-mode-local-vars-hook #'tree-sitter! 'append))

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

  (when (modulep! :ui workspaces)
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
  :when (modulep! +lsp)
  :unless (modulep! :tools lsp +eglot)
  :after lsp-mode
  :preface (setq lsp-julia-default-environment nil)
  :init
  ;; If no environment is set, then auto-detect one in ~/.julia/environments/,
  ;; falling back to `lsp-julia-default-environment's default.
  (unless lsp-julia-default-environment
    (setq lsp-julia-default-environment
          (or (car (last (doom-glob "~/.julia/environments/v*")))
              "~/.julia/environments/v1.6"))))


(use-package! eglot-jl
  :when (modulep! +lsp)
  :when (modulep! :tools lsp +eglot)
  :after eglot
  :preface
  ;; Prevent auto-install of LanguageServer.jl
  (setq eglot-jl-language-server-project
        (or (car (last (doom-glob "~/.julia/environments/v*")))
            "~/.julia/environments/v1.6"))
  :init
  ;; Prevent timeout while installing LanguageServer.jl
  (setq-hook! 'julia-mode-hook eglot-connect-timeout (max eglot-connect-timeout 60))
  :config (eglot-jl-init))


(use-package! julia-snail
  :when (modulep! +snail)
  :when (modulep! :term vterm)
  :hook (julia-mode . julia-snail-mode)
  :config
  (setq julia-snail-popup-display-eval-results :command)
  (setq julia-snail-multimedia-enable t)
  (setq julia-snail-popup-display-face '(:background base3 :box `(:line-width -1 :color base5)))

  (set-popup-rule! "^\\*julia.*\\*$" :ttl nil :select nil :quit nil)

  (after! julia-mode
    (set-repl-handler! 'julia-mode #'+julia/open-snail-repl
      :persist t
      ;; FIXME These aren't working as expected
      :send-region #'julia-snail-send-region
      :send-buffer #'julia-snail-send-buffer-file))

  (map! (:localleader
         (:map (julia-snail-mode-map)
               "'" #'julia-snail
               "a" #'julia-snail-package-activate
               "r" #'julia-snail-update-module-cache
               "d" #'julia-snail-doc-lookup
               (:prefix ("e" . "eval")
                        "b" #'julia-snail-send-buffer-file
                        "l" #'julia-snail-send-line
                        "r" #'julia-snail-send-region
                        "e" #'julia-snail-send-dwim))
         (:map (julia-snail-repl-mode-map)
               "a" #'julia-snail-package-activate
               "d" #'julia-snail-doc-lookup
               "m" #'julia-snail-repl-go-back
               "r" #'julia-snail-update-module-cache))))
