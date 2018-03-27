;;; lang/elixir/config.el -*- lexical-binding: t; -*-

(def-package! elixir-mode
  :mode "\\.exs?$"
  :mode "\\.elixir2$"
  :config
  ;; disable standard config; more disruptive than it needs to be
  (map-delete sp-pairs 'elixir-mode)
  ;; only complete the basics
  (sp-with-modes 'elixir-mode
    (sp-local-pair "do" "end"
                   :when '(("RET" "<evil-ret>"))
                   :unless '(sp-in-comment-p sp-in-string-p)
                   :skip-match 'sp-elixir-skip-def-p
                   :post-handlers '("||\n[i]"))
    (sp-local-pair "do " " end" :unless '(sp-in-comment-p sp-in-string-p))
    (sp-local-pair "fn " " end" :unless '(sp-in-comment-p sp-in-string-p))))


(def-package! alchemist
  :hook (elixir-mode . alchemist-mode)
  :config
  (set! :lookup 'elixir-mode
    :definition #'alchemist-goto-definition-at-point
    :documentation #'alchemist-help-search-at-point)
  (set! :eval 'elixir-mode #'alchemist-eval-region)
  (set! :repl 'elixir-mode #'alchemist-iex-project-run))


(def-package! alchemist-company
  :when (featurep! :completion company)
  :after elixir-mode
  :config
  ;; Let Doom handle this
  (let ((fn (byte-compile (lambda () (add-to-list (make-local-variable 'company-backends) 'alchemist-company)))))
    (remove-hook 'alchemist-mode-hook fn)
    (remove-hook 'alchemist-iex-mode-hook fn))

  (set! :company-backend 'elixir-mode '(alchemist-company company-yasnippet)))

