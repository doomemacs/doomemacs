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
                   "[<>!]=?" OR
                   "\\.[/*^']" OR
                   "==" OR
                   "=>" OR
                   "\\<xor\\>" OR
                   "[-+*\\/^&|$]=?" OR  ; this has to come before next (updating operators)
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
  :when (featurep! +lsp)
  :after lsp-clients
  :preface
  (setq lsp-julia-default-environment "~/.julia/environments/v1.0")
  (when (featurep! +lsp)
    (add-hook 'julia-mode-local-vars-hook #'lsp!)))
