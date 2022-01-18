;;; lang/crystal/config.el -*- lexical-binding: t; -*-

(after! crystal-mode
  (set-lookup-handlers! 'crystal-mode
    :definition #'crystal-def-jump
    :references #'crystal-tool-imp)
  (when (featurep! +lsp)
    (add-hook 'crystal-mode-local-vars-hook #'lsp!))
  (map! :localleader
        :map crystal-mode-map
        :prefix "t"
        "a" #'crystal-spec-all
        "v" #'crystal-spec-buffer
        "s" #'crystal-spec-line
        "t" #'crystal-spec-switch))


(use-package! flycheck-crystal
  :when (featurep! :checkers syntax)
  :after crystal-mode)


(use-package! flycheck-ameba
  :when (featurep! :checkers syntax)
  :after crystal-mode
  :config (flycheck-ameba-setup))


(use-package! inf-crystal
  :commands crystal-switch-to-inf)
