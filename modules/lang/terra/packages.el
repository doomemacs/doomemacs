;; -*- no-byte-compile: t; -*-
;;; lang/lua/packages.el

(package! terra-mode :recipe (:fetcher github :repo "StanfordLegion/terra-mode"))

(when (featurep! :completion company)
  (package! company-lua))

