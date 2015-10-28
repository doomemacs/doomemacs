;;; module-csharp.el

(use-package csharp-mode
  :functions (csharp-log)
  :mode "\\.cs$"
  :init (add-hook! csharp-mode 'flycheck-mode)
  :config (require 'omnisharp))

;; unity shaders
(use-package shader-mode :mode "\\.shader$")

(use-package omnisharp
  :defer t
  :preface
  (setq omnisharp-server-executable-path "~/Dropbox/lib/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe"
        omnisharp-auto-complete-want-documentation nil)
  :if (file-exists-p omnisharp-server-executable-path)
  :init
  (add-hook! csharp-mode '(emr-initialize omnisharp-mode))
  :config
  (evil-define-key 'normal omnisharp-mode-map
    (kbd "gd")  'omnisharp-go-to-definition
    (kbd ",tr") (λ (omnisharp-unit-test "fixture"))
    (kbd ",ts") (λ (omnisharp-unit-test "single"))
    (kbd ",ta") (λ (omnisharp-unit-test "all")))

  (after! company
    (define-company-backend! csharp-mode (omnisharp))
    (add-hook! csharp-mode 'turn-on-eldoc-mode))

  ;; Map all refactor commands (see emr)
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
