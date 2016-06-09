;;; module-csharp.el --- -*- no-byte-compile: t; -*-

(use-package csharp-mode
  :mode "\\.cs$"
  :init (add-hook 'csharp-mode-hook 'flycheck-mode))

(use-package shader-mode :mode "\\.shader$") ; unity shaders

(use-package omnisharp
  :commands (omnisharp-mode)
  :preface
  (setq omnisharp-auto-complete-want-documentation nil
        omnisharp-server-executable-path (concat doom-ext-dir "/OmniSharp.exe"))
  :when (file-exists-p omnisharp-server-executable-path)
  :init (add-hook! csharp-mode '(eldoc-mode omnisharp-mode))
  :config
  (def-company-backend! csharp-mode (omnisharp))
  (map! :map omnisharp-mode-map
        "gd" 'omnisharp-go-to-definition
        (:localleader
          "tr" (λ! (omnisharp-unit-test "fixture"))
          "ts" (λ! (omnisharp-unit-test "single"))
          "ta" (λ! (omnisharp-unit-test "all"))))

  ;; Map all refactor commands (see emr)
  (mapc (lambda (x)
          (let ((command-name (car x))
                (title (cadr x)))
            (emr-declare-command
             (intern (format "omnisharp-%s" (symbol-name command-name)))
              :title title :modes 'omnisharp-mode)))
        '((find-usages                                 "find usages")
          (find-implementations                        "find implementations")
          (fix-code-issue-at-point                     "fix code issue at point")
          (fix-usings                                  "fix usings")
          (rename                                      "rename")
          (current-type-information                    "current type information")
          (current-type-documentation                  "current type documentation")
          (navigate-to-current-file-member             "navigate to current file member")
          (navigate-to-solution-member                 "navigate to solution member")
          (navigate-to-solution-file-then-file-member  "navigate to solution file then member")
          (navigate-to-solution-file                   "navigate to solution file")
          (navigate-to-region                          "navigate to region")
          (show-last-auto-complete-result              "last auto complete result")
          (show-overloads-at-point                     "show overloads at point")
          (recompile                                   "recompile"))))

(provide 'module-csharp)
;;; module-csharp.el ends here
