;; -*- no-byte-compile: t; -*-
;;; lang/lua/packages.el

(package! terra-mode
  :recipe (:host github :repo "StanfordLegion/terra-mode")
  :pin "eab9e59ded8d4d0b280d5b58ff416366c26be57c")

(when (featurep! :completion company)
  (package! company-lua :pin "29f6819de4d691e5fd0b62893a9f4fbc1c6fcb52"))
