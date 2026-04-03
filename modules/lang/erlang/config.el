;;; lang/erlang/config.el -*- lexical-binding: t; -*-

(use-package! erlang
  :mode ("\\.erlang\\'" . erlang-mode)
  :mode ("/rebar\\.config\\(?:\\.script\\)?\\'" . erlang-mode)
  :mode ("/\\(?:app\\|sys\\)\\.config\\'" . erlang-mode)
  :config
  (set-formatter! 'erlfmt '("rebar3" "format")
    :modes '(erlang-mode erlang-ts-mode))
  (set-repl-handler! '(erlang-mode erlang-ts-mode) #'inferior-erlang)
  (when (modulep! +lsp)
    (add-hook 'erlang-mode-local-vars-hook #'lsp! 'append)
    (add-hook 'erlang-ts-mode-local-vars-hook #'lsp! 'append)))


(use-package! erlang-ts
  :when (modulep! +tree-sitter)
  :defer t
  :init
  (set-tree-sitter! 'erlang-mode 'erlang-ts-mode
    '((erlang :url "https://github.com/WhatsApp/tree-sitter-erlang"))))


(use-package! erlang-flymake
  :when (modulep! :checkers syntax)
  :hook (erlang-mode . erlang-flymake-setup)
  :hook (erlang-ts-mode . erlang-flymake-setup)
  :config
  ;; Leave it to :checkers syntax to auto-actiate
  (remove-hook 'erlang-mode-hook #'flymake-mode))
