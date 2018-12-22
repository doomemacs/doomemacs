;;; lang/elixir/config.el -*- lexical-binding: t; -*-

(def-package! elixir-mode
  :defer t
  :init
  ;; Disable default smartparens config; there are too many, they're intrusive
  ;; and we only want a subset of them (defined below).
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

  (set-pretty-symbols! 'elixir-mode
    ;; Functional
    :def "def"
    :lambda "fn"
    ;; :src_block "do"
    ;; :src_block_end "end"
    ;; Flow
    :not "!"
    :in "in" :not-in "not in"
    :and "and" :or "or"
    :for "for"
    :return "return" :yield "use")

  (def-package! alchemist-company
    :when (featurep! :completion company)
    :commands alchemist-company
    :init
    (set-company-backend! 'elixir-mode '(alchemist-company company-yasnippet))
    :config
    ;; Alchemist doesn't use hook symbols to add these backends, so we have to
    ;; use the entire closure to get rid of it.
    (let ((fn (byte-compile (lambda () (add-to-list (make-local-variable 'company-backends) 'alchemist-company)))))
      (remove-hook 'alchemist-mode-hook fn)
      (remove-hook 'alchemist-iex-mode-hook fn)))

  (def-package! flycheck-credo
    :when (featurep! :feature syntax-checker)
    :config (flycheck-credo-setup)))


(def-package! alchemist
  :hook (elixir-mode . alchemist-mode)
  :config
  (set-lookup-handlers! 'elixir-mode
    :definition #'alchemist-goto-definition-at-point
    :documentation #'alchemist-help-search-at-point)
  (set-eval-handler! 'elixir-mode #'alchemist-eval-region)
  (set-repl-handler! 'elixir-mode #'alchemist-iex-project-run))
