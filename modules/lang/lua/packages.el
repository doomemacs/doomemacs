;; -*- no-byte-compile: t; -*-
;;; lang/lua/packages.el

(package! lua-mode :pin "2d9a468b94acd8480299d47449b53136060b7b23")

(when (featurep! +moonscript)
  (package! moonscript :pin "56f90471e2ced2b0a177aed4d8c2f854797e9cc7")
  (when (featurep! :checkers syntax)
    (package! flycheck-moonscript
      :recipe (:host github :repo "hlissner/emacs-flycheck-moonscript")
      :pin "fcb99e5efcf31db05f236f02eaa575986a57172d")))

(when (featurep! +fennel)
  (package! fennel-mode :pin "ba14a7ddc21f8bc2b9bc58abd1655abbe12118d7"))

(when (featurep! :completion company)
  (package! company-lua :pin "29f6819de4d691e5fd0b62893a9f4fbc1c6fcb52"))
