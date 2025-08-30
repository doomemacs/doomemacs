;;; lang/scala/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "build.sbt"))


;;
;;; Packages

(defun +scala-common-config (mode)
  (set-formatter! 'scalafmt '("scalafmt" "--stdin")
    :modes (list mode))
  (set-repl-handler! mode #'+scala/open-repl
    :persist t)
  (set-ligatures! mode
    ;; Functional
    :def "def"
    :composition  "compose"
    ;; HKT
    :lambda       "Lambda"
    ;; Types
    :null         "none"
    :null         "None"
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

  (when (modulep! +lsp)
    (add-hook (intern (format "%s-local-vars-hook" mode)) #'lsp! 'append)))


(after! scala-mode
  (setq scala-indent:align-parameters t
        ;; indent block comments to first asterix, not second
        scala-indent:use-javadoc-style t)

  (setq-hook! 'scala-mode-hook
    comment-line-break-function #'+scala-comment-indent-new-line-fn
    lsp-enable-indentation nil)

  (+scala-common-config 'scala-mode))


(use-package! scala-ts-mode
  :when (modulep! +tree-sitter)
  :when (fboundp 'treesit-available-p)
  :defer t
  :init
  (set-tree-sitter! 'scala-mode 'scala-ts-mode
    '((scala :url "https://github.com/tree-sitter/tree-sitter-scala")))

  :config
  (when (modulep! +lsp)
    (setq-hook! 'scala-ts-mode-hook lsp-enable-indentation nil))

  (+scala-common-config 'scala-ts-mode))
