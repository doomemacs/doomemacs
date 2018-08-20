;;; tools/lsp/config.el -*- lexical-binding: t; -*-

(def-package! lsp-mode
  :commands (lsp-mode lsp-define-stdio-client)
  :config
  (setq lsp-enable-eldoc t
        lsp-eldoc-render-all nil
        lsp-highlight-symbol-at-point nil
        lsp-enable-completion-at-point t))

(def-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (with-eval-after-load "lsp-mode"
    (setq lsp-ui-flycheck-enable t))
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-symbol t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-update-mode 'point)
  (setq lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 35
        lsp-ui-sideline-ignore-duplicate t)
  (set! :lookup 'lsp-ui-mode
    :definition #'lsp-ui-peek-find-definitions
    :references #'lsp-ui-peek-find-references))

(def-package! company-lsp
  :after-call lsp-mode
  :config
  (set! :company-backend 'lsp-mode '(company-lsp))
  ;; next two might speed things up a bit
  (setq company-lsp-transformers nil)
  (setq company-lsp-cache-candidates nil)
  (setq company-lsp-async t
        company-lsp-enable-recompletion t)
  (when (featurep! :feature snippets)
    (setq company-lsp-enable-snippet t)))

