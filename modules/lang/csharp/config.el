;;; lang/csharp/config.el -*- lexical-binding: t; -*-

(use-package! csharp-mode
  :defer t
  :config
  (set-formatter! 'csharpier '("csharpier" "format" "--write-stdout")
    :modes '(csharp-mode))
  (set-electric! 'csharp-mode :chars '(?\n ?\}))
  (set-rotate-patterns! 'csharp-mode
    :symbols '(("public" "protected" "private")
               ("class" "struct")))
  (set-ligatures! 'csharp-mode
    ;; Functional
    :lambda        "() =>"
    ;; Types
    :null          "null"
    :true          "true"
    :false         "false"
    :int           "int"
    :float         "float"
    :str           "string"
    :bool          "bool"
    :list          "List"
    ;; Flow
    :not           "!"
    :in            "in"
    :and           "&&"
    :or            "||"
    :for           "for"
    :return        "return"
    :yield         "yield")

  (sp-local-pair 'csharp-mode "<" ">"
                 :when '(+csharp-sp-point-in-type-p)
                 :post-handlers '(("| " "SPC")))

  (when (modulep! +lsp)
    (add-hook 'csharp-mode-local-vars-hook #'lsp! 'append))

  (defadvice! +csharp-disable-clear-string-fences-a (fn &rest args)
    "This turns off `c-clear-string-fences' for `csharp-mode'. When
on for `csharp-mode' font lock breaks after an interpolated string
or terminating simple string."
    :around #'csharp-disable-clear-string-fences
    (unless (eq major-mode 'csharp-mode)
      (apply fn args))))


(use-package! csharp-tree-sitter
  :when (modulep! +tree-sitter)
  :defer t
  :init
  (add-hook 'csharp-mode-local-vars-hook #'tree-sitter! 'append)
  (when (fboundp #'csharp-tree-sitter-mode)
    (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode))
    (when (modulep! +lsp)
      (add-hook 'csharp-tree-sitter-mode-local-vars-hook #'lsp! 'append))))


;; Unity shaders
(use-package! shader-mode
  :when (modulep! +unity)
  :mode "\\.shader\\'"
  :config
  (def-project-mode! +csharp-unity-mode
    :modes '(csharp-mode shader-mode)
    :files (and "Assets" "Library/MonoManager.asset" "Library/ScriptMapper")))


(use-package! sharper
  :when (modulep! +dotnet)
  :general ("C-c d" #'sharper-main-transient)
  :config
  (map! (:map sharper--solution-management-mode-map
         :nv "RET" #'sharper-transient-solution
         :nv "gr" #'sharper--solution-management-refresh)
        (:map sharper--project-references-mode-map
         :nv "RET" #'sharper-transient-project-references
         :nv "gr" #'sharper--project-references-refresh)
        (:map sharper--project-packages-mode-map
         :nv "RET" #'sharper-transient-project-packages
         :nv "gr" #'sharper--project-packages-refresh)
        (:map sharper--nuget-results-mode-map
         :nv "RET" #'sharper--nuget-search-install)))


(use-package! sln-mode :mode "\\.sln\\'")
