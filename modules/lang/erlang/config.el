;;; private/erlang/config.el -*- lexical-binding: t; -*-

(def-package! erlang
  ;; customizations
  :mode "\\.erlang$"
  ;; rebar files
  :mode "/rebar\\.config\\(?:\\.script\\)?$"
  ;; erlang configs
  :mode "/\\(?:app\\|sys\\)\\.config$")

(def-package! flycheck-rebar3
  :when (featurep! :feature syntax-checker)
  :after erlang
  :config
  (flycheck-rebar3-setup))

;; Completion via Ivy
(def-package! ivy-erlang-complete
  :when (featurep! :completion ivy)
  :hook (erlang-mode . ivy-erlang-complete-init)
  :config
  (add-hook! 'erlang-mode-hook
    (add-hook 'after-save-hook #'ivy-erlang-complete-reparse nil t)))


;; Completion via Company
(def-package! company-erlang
  :when (featurep! :completion company)
  :hook (erlang-mode . company-erlang-init))
