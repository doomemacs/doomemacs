;;; lang/json/config.el -*- lexical-binding: t; -*-

(use-package! json-mode
  :mode "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'"
  :init
  (when (featurep! +lsp)
    (add-hook 'json-mode-local-vars-hook #'lsp!))
  :config
  (set-electric! 'json-mode :chars '(?\n ?: ?{ ?}))

  (map! :after json-mode
        :map json-mode-map
        :localleader
        "p" #'jsons-print-path))



(use-package! counsel-jq
  :when (featurep! :completion ivy)
  :defer t
  :init
  (map! :after json-mode
        :map json-mode-map
        :localleader
        "s" #'counsel-jq))
