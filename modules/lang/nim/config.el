;;; lang/nim/config.el -*- lexical-binding: t; -*-

(after! nim-mode
  (defun +nim|init-nimsuggest-mode ()
    "Conditionally load `nimsuggest-mode', instead of clumsily erroring out if
nimsuggest isn't installed."
    (when (file-executable-p nimsuggest-path)
      (nimsuggest-mode)))
  (add-hook 'nim-mode-hook #'+nim|init-nimsuggest-mode)

  (map! :map nim-mode-map
        :localleader
        :n "b" #'nim-compile))


(def-package! flycheck-nim
  :when (featurep! :feature syntax-checker)
  :after nim-mode)

