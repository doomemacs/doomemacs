;;; lang/elixir/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "mix.exs"))


;;
;;; Packages

(use-package! elixir-mode
  :defer t
  :init
  ;; Disable default smartparens config. There are too many pairs; we only want
  ;; a subset of them (defined below).
  (provide 'smartparens-elixir)
  :config
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

  ;; ...and only complete the basics
  (sp-with-modes 'elixir-mode
    (sp-local-pair "do" "end"
                   :when '(("RET" "<evil-ret>"))
                   :unless '(sp-in-comment-p sp-in-string-p)
                   :post-handlers '("||\n[i]"))
    (sp-local-pair "do " " end" :unless '(sp-in-comment-p sp-in-string-p))
    (sp-local-pair "fn " " end" :unless '(sp-in-comment-p sp-in-string-p)))

  (when (featurep! +lsp)
    (add-hook 'elixir-mode-local-vars-hook #'lsp!))

  (use-package! flycheck-credo
    :when (featurep! :checkers syntax)
    :config (flycheck-credo-setup)))


(use-package! alchemist
  :hook (elixir-mode . alchemist-mode)
  :init
  (after! elixir-mode
    (set-lookup-handlers! 'elixir-mode
      :definition #'alchemist-goto-definition-at-point
      :documentation #'alchemist-help-search-at-point)
    (set-eval-handler! 'elixir-mode #'alchemist-eval-region)
    (set-repl-handler! 'elixir-mode #'alchemist-iex-project-run)))


(use-package! alchemist-company
  :when (featurep! :completion company)
  :commands alchemist-company
  :init
  (after! elixir-mode
    (set-company-backend! 'elixir-mode '(alchemist-company company-yasnippet)))
  :config
  ;; Alchemist doesn't use hook symbols to add these backends, so we have to use
  ;; the entire closure to get rid of it.
  (let ((fn (byte-compile (lambda () (add-to-list (make-local-variable 'company-backends) 'alchemist-company)))))
    (remove-hook 'alchemist-mode-hook fn)
    (remove-hook 'alchemist-iex-mode-hook fn)))
