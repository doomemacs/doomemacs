;;; lang/json/config.el -*- lexical-binding: t; -*-

(use-package! json-mode
  :mode "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'"
  :init
  (when (modulep! +lsp)
    (add-hook 'json-mode-local-vars-hook #'lsp! 'append))
  (when (modulep! +tree-sitter)
    (add-hook! '(json-mode-local-vars-hook
                 jsonc-mode-local-vars-hook)
               :append #'tree-sitter!))
  :config
  (set-electric! 'json-mode :chars '(?\n ?: ?{ ?}))

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



(use-package! counsel-jq
  :when (modulep! :completion ivy)
  :defer t
  :init
  (map! :after json-mode
        :map json-mode-map
        :localleader
        "s" #'counsel-jq))
