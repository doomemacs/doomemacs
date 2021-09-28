;; -*- no-byte-compile: t; -*-
;;; lang/lua/packages.el

(package! terra-mode
  :recipe (:host github :repo "StanfordLegion/terra-mode")
  :pin "ceef8cae5bddc70ee3d5d4d00aa323e3cd6a11be")

(when (featurep! :completion company)
  (package! company-lua :pin "29f6819de4d691e5fd0b62893a9f4fbc1c6fcb52"))
