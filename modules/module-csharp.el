;;; module-csharp.el

(use-package csharp-mode
  :functions (csharp-log)
  :mode "\\.cs$"
  :init (add-hook! csharp-mode 'flycheck-mode))

;; unity shaders
(use-package shader-mode :mode "\\.shader$")

(use-package omnisharp
  :after csharp-mode
  :preface
  (setq omnisharp-server-executable-path "~/Dropbox/lib/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe"
        omnisharp-auto-complete-want-documentation nil)
  :if (file-exists-p omnisharp-server-executable-path)
  :init
  (add-hook! csharp-mode '(emr-initialize omnisharp-mode))
  :config

  (bind! :map omnisharp-mode-map
         :n "gd" 'omnisharp-go-to-definition
         (:prefix ","
           :n "tr" (λ (omnisharp-unit-test "fixture"))
           :n "ts" (λ (omnisharp-unit-test "single"))
           :n "ta" (λ (omnisharp-unit-test "all"))))

  (after! company
    (add-company-backend! csharp-mode (omnisharp))
    (add-hook! csharp-mode 'turn-on-eldoc-mode))

  (mapc (lambda (x)
          (let ((command-name (car x))
                (title (cadr x)))
            (emr-declare-command (intern (format "omnisharp-%s" (symbol-name command-name)))
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
