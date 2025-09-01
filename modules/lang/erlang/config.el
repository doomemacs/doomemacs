;;; lang/erlang/config.el -*- lexical-binding: t; -*-

(use-package! erlang
  :mode ("\\.erlang\\'" . erlang-mode)
  :mode ("/rebar\\.config\\(?:\\.script\\)?\\'" . erlang-mode)
  :mode ("/\\(?:app\\|sys\\)\\.config\\'" . erlang-mode)
  :config
  (set-formatter! 'erlfmt '("rebar3" "fmt" "-")
    :modes '(erlang-mode erlang-ts-mode))
  (when (modulep! +lsp)
    (add-hook 'erlang-mode-local-vars-hook #'lsp! 'append)))


(use-package! erlang-ts
  :when (modulep! +tree-sitter)
  :defer t
  :init
  (set-tree-sitter! 'erlang-mode 'erlang-ts-mode
    '((erlang :url "https://github.com/WhatsApp/tree-sitter-erlang"))))
