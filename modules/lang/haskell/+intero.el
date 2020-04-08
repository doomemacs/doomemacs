;;; lang/haskell/+intero.el -*- lexical-binding: t; -*-
;;;###if (featurep! +intero)

(use-package! intero
  :commands intero-mode
  :hook (haskell-mode-local-vars . +haskell-init-intero-h)
  :config
  (defun +haskell-init-intero-h ()
    "Initializes `intero-mode' in haskell-mode, unless stack isn't installed.
This is necessary because `intero-mode' doesn't do its own error checks."
    (when (derived-mode-p 'haskell-mode)
      (if (executable-find "stack")
          (intero-mode +1)
        (message "Couldn't find stack. Refusing to enable intero-mode."))))

  (setq haskell-compile-cabal-build-command "stack build --fast")
  (set-lookup-handlers! 'intero-mode :definition #'intero-goto-definition)
  (set-company-backend! 'intero-mode 'intero-company)
  (when (featurep! :checkers syntax)
    (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

  (when (featurep 'evil)
    (add-hook 'intero-mode-hook #'evil-normalize-keymaps))
  (map! :localleader
        :map intero-mode-map
        "t" #'intero-type-at
        "i" #'intero-info
        "l" #'intero-repl-load
        "e" #'intero-repl-eval-region
        "a" #'intero-apply-suggestions))
