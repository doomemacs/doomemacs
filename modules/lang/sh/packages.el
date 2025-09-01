;; -*- no-byte-compile: t; -*-
;;; lang/sh/packages.el

(unless (modulep! +lsp)
  (when (modulep! :completion company)
    (package! company-shell :pin "5f959a63a6e66eb0cbdac3168cad523a62cc2ccd"))
  (when (modulep! :completion corfu)
    (package! bash-completion :pin "762f28fefba487e15d626691310f3194804eb71a")))

(when (modulep! +fish)
  (package! fish-mode :pin "2526b1803b58cf145bc70ff6ce2adb3f6c246f89"))

(when (modulep! +powershell)
  (package! powershell :pin "99e0e73082fd48314a9825254dac45f318e5bb59"))
