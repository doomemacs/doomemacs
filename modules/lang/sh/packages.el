;; -*- no-byte-compile: t; -*-
;;; lang/sh/packages.el

(when (featurep! :completion company)
  (package! company-shell :pin "a77f4de75912aa87314cde92c603b831d5050246"))

(when (featurep! +fish)
  (package! fish-mode :pin "a7c953b1491ac3a3e00a7b560f2c9f46b3cb5c04"))

(when (featurep! +powershell)
  (package! powershell :pin "ce1f0ae0b2e41cd0934a9dfbf2ff016b1d14e9c0"))
