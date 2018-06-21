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
  (set-lookup-handlers! 'intero-mode :definition #'intero-goto-definition))


(def-package! hindent
  :hook (haskell-mode . hindent-mode))
