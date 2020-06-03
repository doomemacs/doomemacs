;; -*- no-byte-compile: t; -*-
;;; lang/lua/packages.el

(package! terra-mode
  :recipe (:host github :repo "StanfordLegion/terra-mode")
  :pin "1e5e82410d")

(when (featurep! :completion company)
  (package! company-terraform
    :recipe (:host github :repo "rafalcieslak/emacs-company-terraform")
    :pin "2d11a21"))
