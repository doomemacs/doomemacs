;;; lang/nim/config.el -*- lexical-binding: t; -*-

(def-package! nim-mode
  :mode "\\.nim\\'"
  :mode ("\\.nim\\(ble\\|s\\)\\'" . nimscript-mode)
  :config
  (load "nim-mode-autoloads" nil t)
  ;; NOTE nim-mode autoloads sets up xref

  (defun +nim|init-nimsuggest-mode ()
    "Conditionally load `nimsuggest-mode', instead of clumsily erroring out if
nimsuggest isn't installed."
    (when (executable-find "nimsuggest")
      (nimsuggest-mode)))
  (add-hook 'nim-mode-hook #'+nim|init-nimsuggest-mode)

  (map! :map nim-mode-map
        :localleader
        :n "b" #'+nim/build-menu)

  (def-menu! +nim/build-menu
    "Building commands for `nim-mode' buffers."
    '(("Build & run" :exec nim-compile))
    :prompt "Build"))


(def-package! flycheck-nim
  :when (featurep! :feature syntax-checker)
  :after nim-mode
  :config
  (add-hook 'nimsuggest-mode-hook #'flycheck-mode))

