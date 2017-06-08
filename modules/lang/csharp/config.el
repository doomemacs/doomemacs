;;; lang/csharp/config.el -*- lexical-binding: t; -*-

(def-package! csharp-mode :mode "\\.cs$")


(def-package! omnisharp
  :after csharp-mode
  :preface
  (setq omnisharp-auto-complete-want-documentation nil
        omnisharp-server-executable-path (concat doom-local-dir "OmniSharp.exe"))
  :config
  (if (file-exists-p omnisharp-server-executable-path)
      (add-hook! csharp-mode #'(eldoc-mode flycheck-mode omnisharp-mode))
    (warn "csharp-mode: omnisharp server isn't installed, completion won't work"))

  (set! :company-backend 'csharp-mode '(company-omnisharp))

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

