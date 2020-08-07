;;; lang/windows-scripts/config.el -*- lexical-binding: t; -*-

(use-package lsp-mode
  :when (featurep! +lsp)
  :hook ((powershell-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package! powershell
  :mode (("\\.ps1\\'" . powershell-mode)
         ("\\.psm1\\'" . powershell-mode))
  :config
  (map! :localleader
        :map powershell-mode-map
         "r"   #'powershell-regexp-to-regex
         "q"   #'powershell-quote-selection
         "d"   #'powershell-dollarparen-selection))

(use-package! dos
  :mode (("\\.bat\\'" . dos-mode)
         ("\\.cmd\\'" . dos-mode)))
