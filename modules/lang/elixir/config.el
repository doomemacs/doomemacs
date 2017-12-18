;;; lang/elixir/config.el -*- lexical-binding: t; -*-

(def-package! elixir-mode
  :mode "\\.exs?$"
  :mode "\\.elixir2$"
  :config
  ;; disable standard config; more disruptive than it needs to be
  (dolist (beg '("fn" "do" "def" "defp" "defmodule" "if" "unless" "case" "receive"))
    (sp-local-pair 'elixir-mode beg nil :actions :rem))
  ;; only complete the basics
  (sp-with-modes 'elixir-mode
    (sp-local-pair "do" "end" :when '(("RET" "<evil-ret>")) :post-handlers '("||\n[i]"))
    (sp-local-pair "do " " end")
    (sp-local-pair "fn " " end")))


(def-package! alchemist
  :after elixir-mode
  :hook (elixir-mode . alchemist-mode)
  :config
  (set! :jump 'elixir-mode
    :definition #'alchemist-goto-definition-at-point
    :documentation #'alchemist-help-search-at-point)
  (set! :eval 'elixir-mode #'alchemist-eval-region))


(def-package! alchemist-company
  :when (featurep! :completion company)
  :after elixir-mode
  :config
  ;; Let Doom handle this
  (let ((fn (byte-compile (lambda () (add-to-list (make-local-variable 'company-backends) 'alchemist-company)))))
    (remove-hook 'alchemist-mode-hook fn)
    (remove-hook 'alchemist-iex-mode-hook fn))

  (set! :company-backend 'elixir-mode '(alchemist-company company-yasnippet)))

