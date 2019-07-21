;; -*- no-byte-compile: t; -*-
;;; lang/lua/packages.el

(package! terra-mode
  :recipe (:host github :repo "StanfordLegion/terra-mode"))

(when (featurep! :completion company)
  (package! company-lua))

