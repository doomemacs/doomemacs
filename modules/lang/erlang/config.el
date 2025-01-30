;;; lang/erlang/config.el -*- lexical-binding: t; -*-

(use-package! erlang
  :mode ("\\.erlang\\'" . erlang-mode)
  :mode ("/rebar\\.config\\(?:\\.script\\)?\\'" . erlang-mode)
  :mode ("/\\(?:app\\|sys\\)\\.config\\'" . erlang-mode)
  :config
  (set-formatter! 'erlfmt '("rebar3" "fmt" "-") :modes '(erlang-mode))
  (set-popup-rules! '(("^\\*erlang\\*" :size 0.30 :quit nil :ttl nil)))
  (when (modulep! +lsp)
    (add-hook 'erlang-mode-local-vars-hook #'lsp! 'append))

  (when (modulep! +tree-sitter)
    (add-hook 'erlang-mode-local-vars-hook #'tree-sitter! 'append)))
