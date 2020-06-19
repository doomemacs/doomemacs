;;; lang/csharp/config.el -*- lexical-binding: t; -*-

(use-package! csharp-mode
  :hook (csharp-mode . rainbow-delimiters-mode)
  :config
  (set-electric! 'csharp-mode :chars '(?\n ?\}))
  (set-rotate-patterns! 'csharp-mode
    :symbols '(("public" "protected" "private")
               ("class" "struct")))

  (sp-local-pair 'csharp-mode "<" ">"
                 :when '(+csharp-sp-point-in-type-p)
                 :post-handlers '(("| " "SPC")))

  (when (featurep! +lsp)
    (add-hook 'csharp-mode-local-vars-hook #'lsp!))

  (defadvice! +csharp-disable-clear-string-fences-a (orig-fn &rest args)
    "This turns off `c-clear-string-fences' for `csharp-mode'. When
on for `csharp-mode' font lock breaks after an interpolated string
or terminating simple string."
    :around #'csharp-disable-clear-string-fences
    (unless (eq major-mode 'csharp-mode)
      (apply orig-fn args))))


(use-package! omnisharp
  :unless (featurep! +lsp)
  :commands omnisharp-install-server
  :hook (csharp-mode-local-vars . omnisharp-mode)
  :preface
  (setq omnisharp-auto-complete-want-documentation nil
        omnisharp-cache-directory (concat doom-etc-dir "omnisharp"))
  :config
  (set-company-backend! 'omnisharp-mode 'company-omnisharp)
  (set-lookup-handlers! 'omnisharp-mode
    :definition #'omnisharp-go-to-definition
    :references #'omnisharp-find-usages
    :documentation #'omnisharp-current-type-documentation)

  ;; Kill the omnisharp server once the last csharp-mode buffer is killed
  (add-hook! 'omnisharp-mode-hook
    (add-hook 'kill-buffer-hook #'+csharp-kill-omnisharp-server-h nil t))

  (map! :localleader
        :map omnisharp-mode-map
        "b" #'omnisharp-recompile
        (:prefix "r"
          "u"  #'omnisharp-fix-usings
          "r"  #'omnisharp-rename
          "a"  #'omnisharp-show-last-auto-complete-result
          "o"  #'omnisharp-show-overloads-at-point)
        (:prefix "g"
          "u"  #'omnisharp-find-usages
          "i"  #'omnisharp-find-implementations
          "f"  #'omnisharp-navigate-to-current-file-member
          "m"  #'omnisharp-navigate-to-solution-member
          "M"  #'omnisharp-navigate-to-solution-file-then-file-member
          "F"  #'omnisharp-navigate-to-solution-file
          "r"  #'omnisharp-navigate-to-region
          "ti" #'omnisharp-current-type-information
          "td" #'omnisharp-current-type-documentation)
        (:prefix "t"
          "s" #'omnisharp-unit-test-at-point
          "l" #'omnisharp-unit-test-last
          "b" #'omnisharp-unit-test-buffer)))


;; Unity shaders
(use-package! shader-mode
  :when (featurep! +unity)
  :mode "\\.shader\\'"
  :config
  (def-project-mode! +csharp-unity-mode
    :modes '(csharp-mode shader-mode)
    :files (and "Assets" "Library/MonoManager.asset" "Library/ScriptMapper")))
