;;; lang/csharp/config.el -*- lexical-binding: t; -*-

(use-package! csharp-mode
  :hook (csharp-mode . rainbow-delimiters-mode)
  :defer t
  :init
  (when (modulep! +tree-sitter)
    (set-tree-sitter! 'csharp-mode 'csharp-ts-mode
      '((c-sharp :url "https://github.com/tree-sitter/tree-sitter-c-sharp"
                 :rev "v0.23.1"
                 :commit "362a8a41b265056592a0c3771664a21d23a71392"))))
  :config
  (set-formatter! 'csharpier '("csharpier" "format" "--write-stdout")
    :modes '(csharp-mode csharp-ts-mode))
  (set-electric! '(csharp-mode csharp-ts-mode) :chars '(?\n ?\}))
  (set-rotate-patterns! '(csharp-mode csharp-ts-mode)
    :symbols '(("public" "protected" "private")
               ("class" "struct")))
  (set-ligatures! '(csharp-mode csharp-ts-mode)
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

  (sp-local-pair '(csharp-mode csharp-ts-mode) "<" ">"
                 :when '(+csharp-sp-point-in-type-p)
                 :post-handlers '(("| " "SPC")))

  (when (modulep! +lsp)
    (add-hook 'csharp-mode-local-vars-hook #'lsp! 'append)
    (add-hook 'csharp-ts-mode-local-vars-hook #'lsp! 'append))

  (defadvice! +csharp-disable-clear-string-fences-a (fn &rest args)
    "This turns off `c-clear-string-fences' for `csharp-mode'. When
on for `csharp-mode' font lock breaks after an interpolated string
or terminating simple string."
    :around #'csharp-disable-clear-string-fences
    (unless (eq major-mode 'csharp-mode)
      (apply fn args))))


;; Unity shaders
(use-package! shader-mode
  :when (modulep! +unity)
  :mode "\\.shader\\'"
  :config
  (def-project-mode! +csharp-unity-mode
    :modes '(csharp-mode csharp-ts-mode shader-mode)
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
