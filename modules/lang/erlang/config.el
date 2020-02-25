;;; lang/erlang/config.el -*- lexical-binding: t; -*-

(use-package! erlang
  :mode ("\\.erlang\\'" . erlang-mode)
  :mode ("/rebar\\.config\\(?:\\.script\\)?\\'" . erlang-mode)
  :mode ("/\\(?:app\\|sys\\)\\.config\\'" . erlang-mode))


(use-package! flycheck-rebar3
  :when (featurep! :checkers syntax)
  :after flycheck
  :config (flycheck-rebar3-setup))


(use-package! ivy-erlang-complete
  :when (featurep! :completion ivy)
  :hook (erlang-mode . ivy-erlang-complete-init)
  :config
  (add-hook! 'erlang-mode-hook
    (add-hook 'after-save-hook #'ivy-erlang-complete-reparse nil t)))


(use-package! company-erlang
  :when (featurep! :completion company)
  :unless (featurep! +lsp)
  :hook (erlang-mode . company-erlang-init))

(when (featurep! +lsp)
  (add-hook 'erlang-mode-local-vars-hook #'lsp!))
