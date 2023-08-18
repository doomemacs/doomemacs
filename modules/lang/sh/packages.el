;; -*- no-byte-compile: t; -*-
;;; lang/sh/packages.el

(when (modulep! :completion company)
  (package! company-shell :pin "5f959a63a6e66eb0cbdac3168cad523a62cc2ccd"))

(when (modulep! +fish)
  (package! fish-mode :pin "6869efbc05cdb628209338bf57fe52742e53d956"))

(when (modulep! +powershell)
  (package! powershell :pin "f2da15857e430206e215a3c65289b4058ae3c976"))
