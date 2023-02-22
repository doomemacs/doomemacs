;; -*- no-byte-compile: t; -*-
;;; lang/sh/packages.el

(when (modulep! :completion company)
  (package! company-shell :pin "5f959a63a6e66eb0cbdac3168cad523a62cc2ccd"))

(when (modulep! +fish)
  (package! fish-mode :pin "d04478c0aba018cb789d77d591bfe315cb25132a"))

(when (modulep! +powershell)
  (package! powershell :pin "f2da15857e430206e215a3c65289b4058ae3c976"))
