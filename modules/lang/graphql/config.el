;;; lang/graphql/config.el -*- lexical-binding: t; -*-

;;
;;; Packages

(defun +graphql-common-config (mode)
  (if (modulep! +lsp)
      (add-hook (intern (format "%s-local-vars-hook" mode)) #'lsp! 'append)
    (set-company-backend! mode 'company-graphql))

  (set-docsets! mode :add "GraphQL Specification")
  (set-electric! mode
    :chars '(?\} ?\))
    :words '("or" "and"))
  (set-ligatures! mode
    :null "null"
    :true "true" :false "false"
    :int "Int" :str "String"
    :float "Float"
    :bool "Bool"

    :not "not"
    :and "and" :or "or"))


(after! graphql-mode
  (defface nerd-icons-rhodamine
    '((t (:foreground "#E10098")))
    "Face for GraphQL icon."
    :group 'nerd-icons-faces)
  (+graphql-common-config 'graphql-mode))


(use-package! graphql-ts-mode
  :when (modulep! +tree-sitter)
  :defer t
  :init
  (set-tree-sitter! 'graphql-mode 'graphql-ts-mode
    '((graphql :url "https://github.com/bkegley/tree-sitter-graphql")))
  :config
  (+graphql-common-config 'graphql-ts-mode))


(use-package! graphql-doc
  :after graphql-mode)
