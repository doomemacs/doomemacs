;;; private/erlang/config.el -*- lexical-binding: t; -*-

(dolist (regexp '("\\.erlang$"
                  ;; rebar files
                  "/rebar\\.config\\(?:\\.script\\)?$"
                  ;; erlang configs
                  "/\\(?:app\\|sys\\)\\.config$"))
  (add-to-list 'auto-mode-alist (cons regexp 'erlang-mode)))


(def-package! flycheck-rebar3
  :when (featurep! :feature syntax-checker)
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
