;;; lang/haskell/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +haskell|init-intero ()
  "Initializes `intero-mode' in haskell-mode, unless stack isn't installed.
This is necessary because `intero-mode' doesn't do its own error checks."
  (when (derived-mode-p 'haskell-mode)
    (if (executable-find "stack")
        (intero-mode +1)
      (message "Couldn't find stack. Refusing to enable intero-mode."))))
