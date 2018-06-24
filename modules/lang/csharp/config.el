;;; lang/csharp/config.el -*- lexical-binding: t; -*-

;; unity shaders
(add-to-list 'auto-mode-alist '("\\.shader$" . shader-mode))


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
  (add-hook! csharp-mode (add-hook 'kill-buffer-hook #'omnisharp-stop-server nil t))

  (set-company-backend! 'csharp-mode '(company-omnisharp))

  (set-lookup-handlers! 'csharp-mode
    :definition #'omnisharp-go-to-definition
    :references #'omnisharp-find-usages
    :documentation #'omnisharp-current-type-documentation)

  (map! :map omnisharp-mode-map
        :localleader
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
          :n "a" (λ! (omnisharp-unit-test "all")))))

