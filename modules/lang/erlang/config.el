;;; lang/erlang/config.el -*- lexical-binding: t; -*-

(def-package! erlang
  :mode ("\\.erlang$" . erlang-mode)
  :mode ("/rebar\\.config\\(?:\\.script\\)?$" . erlang-mode)
  :mode ("/\\(?:app\\|sys\\)\\.config$" . erlang-mode))


(def-package! flycheck-rebar3
  :when (featurep! :tools flycheck)
  :after flycheck
  :config (flycheck-rebar3-setup))


(def-package! ivy-erlang-complete
  :when (featurep! :completion ivy)
  :hook (erlang-mode . ivy-erlang-complete-init)
  :config
  (add-hook! 'erlang-mode-hook
    (add-hook 'after-save-hook #'ivy-erlang-complete-reparse nil t)))


(def-package! company-erlang
  :when (featurep! :completion company)
  :hook (erlang-mode . company-erlang-init))
