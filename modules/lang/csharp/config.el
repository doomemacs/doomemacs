;;; lang/csharp/config.el -*- lexical-binding: t; -*-

(after! csharp-mode
  (add-hook 'csharp-mode-hook #'rainbow-delimiters-mode)

  (set-electric! 'csharp-mode :chars '(?\n ?\}))
  (set-rotate-patterns! 'csharp-mode
    :symbols '(("public" "protected" "private")
               ("class" "struct")))
  (sp-local-pair 'csharp-mode "<" ">"
                 :when '(+csharp-sp-point-in-type-p)
                 :post-handlers '(("| " "SPC"))))


(def-package! omnisharp
  :hook (csharp-mode . omnisharp-mode)
  :commands omnisharp-install-server
  :preface
  (setq omnisharp-auto-complete-want-documentation nil
        omnisharp-cache-directory (concat doom-cache-dir "omnisharp"))
  :config
  (defun +csharp|cleanup-omnisharp-server ()
    "Clean up the omnisharp server once you kill the last csharp-mode buffer."
    (unless (doom-buffers-in-mode 'csharp-mode (buffer-list))
      (omnisharp-stop-server)))
  (add-hook! csharp-mode
    (add-hook 'kill-buffer-hook #'+csharp|cleanup-omnisharp-server nil t))

  (set-company-backend! 'csharp-mode 'company-omnisharp)
  (set-lookup-handlers! 'csharp-mode
    :definition #'omnisharp-go-to-definition
    :references #'omnisharp-find-usages
    :documentation #'omnisharp-current-type-documentation)

  (map! :localleader
        :map omnisharp-mode-map
        "b" #'omnisharp-recompile
        (:prefix "r"
          "i"  #'omnisharp-fix-code-issue-at-point
          "u"  #'omnisharp-fix-usings
          "r"  #'omnisharp-rename
          "a"  #'omnisharp-show-last-auto-complete-result
          "o"  #'omnisharp-show-overloads-at-point)
        (:prefix "f"
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
          "r" (λ! (omnisharp-unit-test "fixture"))
          "s" (λ! (omnisharp-unit-test "single"))
          "a" (λ! (omnisharp-unit-test "all")))))


(when (featurep! +unity)
  ;; `shader-mode' --- unity shaders
  (add-to-list 'auto-mode-alist '("\\.shader$" . shader-mode))

  (def-project-mode! +csharp-unity-mode
    :modes (csharp-mode shader-mode)
    :files (and "Assets" "Library/MonoManager.asset" "Library/ScriptMapper")))
