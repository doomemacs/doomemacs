;;; lang/scala/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "build.sbt"))


;;
;;; Packages

(after! scala-mode
  (set-pretty-symbols! 'scala-mode
      ;; Functional
      :def "def"
      :composition  "compose"
      ;; HKT
      :lambda       "Lambda"
      ;; Types
      :null         "none"
      :Null         "None"
      :true         "true"
      :false        "false"
      :int          "Int"
      :str          "String"
      :float        "Float"
      :bool         "Boolean"
      :list         "List"
      ;; Flow
      :for          "for"
      :not          "!"
      :and          "&&"
      :or           "||"
      :yield        "yield"
      ;; Other
      :union        "union"
      :intersect    "intersect"
      :diff         "diff")

  (setq scala-indent:align-parameters t
        ;; indent block comments to first asterix, not second
        scala-indent:use-javadoc-style t)
  

  (setq-hook! 'scala-mode-hook
    comment-line-break-function #'+scala-comment-indent-new-line-fn)

  (when (featurep! +lsp)
    (add-hook 'scala-mode-local-vars-hook #'lsp!)))


(use-package! sbt-mode
  :after scala-mode
  :config (set-repl-handler! 'scala-mode #'+scala/open-repl :persist t))
