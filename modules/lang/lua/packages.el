;; -*- no-byte-compile: t; -*-
;;; lang/lua/packages.el

(package! lua-mode)
(package! moonscript)

(when (featurep! :completion company)
  (package! company-lua))

