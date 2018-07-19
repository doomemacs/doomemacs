;;; lang/haskell/+intero.el -*- lexical-binding: t; -*-
;;;###if (featurep! +intero)

(def-package! intero
  :commands intero-mode
  :init
  (defun +haskell|init-intero ()
    "Initializes `intero-mode' in haskell-mode, unless stack isn't installed.
This is necessary because `intero-mode' doesn't do its own error checks."
    (when (derived-mode-p 'haskell-mode)
      (if (executable-find "stack")
          (intero-mode +1)
        (message "Couldn't find stack. Refusing to enable intero-mode."))))
  (add-hook 'haskell-mode-hook #'+haskell|init-intero)
  :config
  (setq haskell-compile-cabal-build-command "stack build --fast")
  (set-lookup-handlers! 'intero-mode :definition #'intero-goto-definition)
  (when (featurep! :feature syntax-checker)
    (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

  (map! :map intero-mode-map
        :localleader
        :n "t" #'intero-type-at
        :n "i" #'intero-info
        :n "l" #'intero-repl-load
        :nv "e" #'intero-repl-eval-region
        :n "a" #'intero-apply-suggestions))
