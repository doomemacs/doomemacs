;;; module-csharp.el

(def-package! csharp-mode
  :mode "\\.cs$"
  :init (add-hook 'csharp-mode-hook #'flycheck-mode))


(def-package! omnisharp
  :commands omnisharp-mode
  :preface
  (setq omnisharp-auto-complete-want-documentation nil
        omnisharp-server-executable-path (concat doom-local-dir "OmniSharp.exe"))
  :when (file-exists-p omnisharp-server-executable-path)
  :init
  (add-hook! csharp-mode #'(eldoc-mode omnisharp-mode))
  :config
  (set! :company-backend 'csharp-mode '(company-omnisharp))

  ;; Map all refactor commands (see emr)
  (map! :map omnisharp-mode-map
        :m "gd" #'omnisharp-go-to-definition

        (:localleader
         :n "b" #'omnisharp-recompile

         (:prefix "r"
           :n "i"  #'omnisharp-fix-code-issue-at-point
           :n "u"  #'omnisharp-fix-usings
           :n "r"  #'omnisharp-rename
           :n "a"  #'omnisharp-show-last-auto-complete-result
           :n "o"  #'omnisharp-show-overloads-at-point)

         (:prefix "f"
           :n "u"  #'omnisharp-find-usages
           :n "i"  #'omnisharp-find-implementations
           :n "f"  #'omnisharp-navigate-to-current-file-member
           :n "m"  #'omnisharp-navigate-to-solution-member
           :n "M"  #'omnisharp-navigate-to-solution-file-then-file-member
           :n "F"  #'omnisharp-navigate-to-solution-file
           :n "r"  #'omnisharp-navigate-to-region
           :n "ti" #'omnisharp-current-type-information
           :n "td" #'omnisharp-current-type-documentation)

         (:prefix "t"
           :n "r" (λ! (omnisharp-unit-test "fixture"))
           :n "s" (λ! (omnisharp-unit-test "single"))
           :n "a" (λ! (omnisharp-unit-test "all"))))))


(def-package! shader-mode :mode "\\.shader$") ; unity shaders

