;; -*- no-byte-compile: t; -*-
;;; lang/sh/packages.el

(unless (modulep! +lsp)
  (when (modulep! :completion company)
    (package! company-shell :pin "5f959a63a6e66eb0cbdac3168cad523a62cc2ccd"))
  (when (modulep! :completion corfu)
    (package! bash-completion :pin "d0637428fd0592ef56baa0255673300129f98c48")))

(when (modulep! +fish)
  (package! fish-mode :pin "2526b1803b58cf145bc70ff6ce2adb3f6c246f89"))

(when (modulep! +powershell)
  (package! powershell :pin "9efa1b4d0a3cc5c0caae166c144a0f76b1d0e5f4"))
