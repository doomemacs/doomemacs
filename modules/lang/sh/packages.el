;; -*- no-byte-compile: t; -*-
;;; lang/sh/packages.el

(unless (modulep! +lsp)
  (when (modulep! :completion company)
    (package! company-shell :pin "5f959a63a6e66eb0cbdac3168cad523a62cc2ccd"))
  (when (modulep! :completion corfu)
    (package! bash-completion :pin "a4c8fbc90221b01d5376ad068d3640350d9130a8")))

(when (modulep! +fish)
  (package! fish-mode :pin "2526b1803b58cf145bc70ff6ce2adb3f6c246f89"))

(when (modulep! +powershell)
  (package! powershell :pin "38727f1cdaf0c937a62b68ee52ec7196b8149f93"))
