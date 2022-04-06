;;; lang/graphql/config.el -*- lexical-binding: t; -*-

(after! graphql-mode
  (defface all-the-icons-rhodamine
    '((t (:foreground "#E10098")))
    "Face for GraphQL icon."
    :group 'all-the-icons-faces)
  ;; Define a doom-modeline compatiable major-mode icon
  (after! all-the-icons
    (setf (alist-get "graphql" all-the-icons-extension-icon-alist)
          '(all-the-icons-fileicon "graphql" :v-adjust -0.05 :face all-the-icons-rhodamine))
    (setf (alist-get "gql" all-the-icons-extension-icon-alist)
          '(all-the-icons-fileicon "graphql" :v-adjust -0.05 :face all-the-icons-rhodamine))
    (setf (alist-get 'graphql-mode all-the-icons-mode-icon-alist)
          '(all-the-icons-fileicon "graphql" :v-adjust -0.05 :face all-the-icons-rhodamine)))
  (if (featurep! +lsp)
      (add-hook 'graphql-mode-local-vars-hook #'lsp! 'append)
    (set-company-backend! 'graphql-mode 'company-graphql))

  (add-hook 'graphql-mode-hook #'rainbow-delimiters-mode)
  (set-docsets! 'graphql-mode :add "GraphQL Specification")

  (set-electric! 'graphql-mode
    :chars '(?\} ?\))
    :words '("or" "and"))

  (set-ligatures! 'graphql-mode
    :null "null"
    :true "true" :false "false"
    :int "Int" :str "String"
    :float "Float"
    :bool "Bool"

    :not "not"
    :and "and" :or "or"))

(use-package! graphql-doc
  :after graphql-mode)
