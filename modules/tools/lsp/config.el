;;; tools/lsp/config.el -*- lexical-binding: t; -*-

(setq lsp-session-file (concat doom-etc-dir "lsp-session")
      lsp-auto-guess-root t
      lsp-keep-workspace-alive nil)

;; Don't prompt to restart LSP servers while quitting Emacs
(add-hook! 'kill-emacs-hook (setq lsp-restart 'ignore))


(def-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-prefer-flymake nil
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 35
        lsp-ui-sideline-ignore-duplicate t)
  (define-key! lsp-ui-mode-map
    [remap xref-find-definitions] #'lsp-ui-peek-find-definitions
    [remap xref-find-references]  #'lsp-ui-peek-find-references)
  (set-lookup-handlers! 'lsp-ui-mode
    :definition #'lsp-ui-peek-find-definitions
    :references #'lsp-ui-peek-find-references))


(def-package! company-lsp
  :when (featurep! :completion company)
  :after lsp-mode
  :config
  (set-company-backend! 'lsp-mode 'company-lsp))
