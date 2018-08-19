;;; lang/elixir/config.el -*- lexical-binding: t; -*-

(def-package! elixir-mode
  :defer t
  :init
  ;; disable default smartparens config
  (provide 'smartparens-elixir)
  :config
  ;; ...and only complete the basics
  (after! smartparens
    (sp-with-modes 'elixir-mode
      (sp-local-pair "do" "end"
                     :when '(("RET" "<evil-ret>"))
                     :unless '(sp-in-comment-p sp-in-string-p)
                     :post-handlers '("||\n[i]"))
      (sp-local-pair "do " " end" :unless '(sp-in-comment-p sp-in-string-p))
      (sp-local-pair "fn " " end" :unless '(sp-in-comment-p sp-in-string-p))))

  (def-package! alchemist-company
    :when (featurep! :completion company)
    :commands alchemist-company
    :init
    (set-company-backend! 'elixir-mode '(alchemist-company company-yasnippet))
    :config
    ;; Alchemist doesn't use hook symbols to add these backends, so we have to use
    ;; the entire closure to get rid of it.
    (let ((fn (byte-compile (lambda () (add-to-list (make-local-variable 'company-backends) 'alchemist-company)))))
      (remove-hook 'alchemist-mode-hook fn)
      (remove-hook 'alchemist-iex-mode-hook fn))))


(def-package! alchemist
  :hook (elixir-mode . alchemist-mode)
  :config
  (set-lookup-handlers! 'elixir-mode
    :definition #'alchemist-goto-definition-at-point
    :documentation #'alchemist-help-search-at-point)
  (set-eval-handler! 'elixir-mode #'alchemist-eval-region)
  (set-repl-handler! 'elixir-mode #'alchemist-iex-project-run))

