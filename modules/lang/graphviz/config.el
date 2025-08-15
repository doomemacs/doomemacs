;;; lang/graphviz/config.el -*- lexical-binding: t; -*-

(use-package! graphviz-dot-mode
  :mode "\\.\\(?:nw\\|rack\\)diag\\'"
  :init
  (after! org-src
    (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot)))
  :config
  (set-company-backend! 'graphviz-dot-mode 'company-graphviz-dot-backend)
  (set-formatter! 'graphviz-dot #'+graphviz-formatter :modes '(graphviz-dot-mode))
  (set-tree-sitter-lang! 'graphviz-dot-mode 'dot)

  (when (modulep! +tree-sitter)
    (add-hook 'graphiz-dot-mode-hook #'tree-sitter!))

  (after! dtrt-indent
    (add-to-list 'dtrt-indent-hook-mapping-list '(graphviz-mode graphviz-dot-indent-width)))

  (when (modulep! :checkers syntax -flymake)
    (after! flycheck
      (eval '(flycheck-define-checker graphviz-dot
               "A checker using graphviz dot."
               :command ("dot")
               :standard-input t
               :error-patterns ((error line-start "Error: <stdin>: " (message "syntax error in line " line (* nonl)))
                                ;; I have no idea if this can actually be printed
                                (error line-start "Error: <stdin>: " (message)))
               :modes graphviz-dot-mode))
      (add-to-list 'flycheck-checkers 'graphviz-dot)))

  (map! :map graphviz-dot-mode-map
        :localleader
        :desc "External view" :nv "e" #'graphviz-dot-view
        :desc "Preview"       :nv "p" #'graphviz-dot-preview
        :prefix ("t" . "toggle")
        :desc "Preview"       :nv "p" #'+graphviz/toggle-preview))
