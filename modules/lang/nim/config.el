;;; lang/nim/config.el -*- lexical-binding: t; -*-

(def-package! nim-mode
  :init
  (add-hook 'nim-mode-hook #'nimsuggest-mode))

(def-package! flycheck-nim
  :when (featurep! :feature syntax-checker)
  :after nim-mode
  :config
  (add-hook 'nimsuggest-mode-hook #'flycheck-mode)

  (map! :map nim-mode-map
        :localleader
        :n "b" #'+nim/build-menu)

  (def-menu! +nim/build-menu
    "Building commands for `nim-mode' buffers."
    '(("Build & run" :exec nim-compile))
    :prompt "Build"))

(when (featurep! :completion company)
  (add-hook 'nimsuggest-mode-hook #'company-mode))
