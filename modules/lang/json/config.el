;;; lang/json/config.el -*- lexical-binding: t; -*-

(use-package! json-mode
  :mode "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'"
  :config
  (set-electric! 'json-mode :chars '(?\n ?: ?{ ?}))

  (when (modulep! +lsp)
    (add-hook 'json-mode-local-vars-hook #'lsp! 'append))

  (map! :after json-mode
        :map json-mode-map
        :localleader
        :desc "Copy path" "p" #'json-mode-show-path
        "t" #'json-toggle-boolean
        "d" #'json-mode-kill-path
        "x" #'json-nullify-sexp
        "+" #'json-increment-number-at-point
        "-" #'json-decrement-number-at-point
        "f" #'json-mode-beautify))


(use-package! json-ts-mode
  :when (modulep! +tree-sitter)
  :when (fboundp 'json-ts-mode) ; 29.1+ only
  :defer t
  :init
  (set-tree-sitter! 'json-mode 'json-ts-mode 'json)
  :config
  ;; HACK: Rely on `major-mode-remap-defaults'.
  (cl-callf2 assq-delete-all 'json-ts-mode auto-mode-alist)

  (when (modulep! +lsp)
    (add-hook 'json-ts-mode-local-vars-hook #'lsp! 'append)))


(use-package! counsel-jq
  :when (modulep! :completion ivy)
  :defer t
  :init
  (map! :after json-mode
        :map json-mode-map
        :localleader
        "s" #'counsel-jq))
