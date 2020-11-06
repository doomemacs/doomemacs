;; -*- no-byte-compile: t; -*-
;;; lang/lua/packages.el

(package! lua-mode :pin "345ebfc1e236d9676e7e9f7364493785e7756348")

(when (featurep! +moonscript)
  (package! moonscript :pin "56f90471e2ced2b0a177aed4d8c2f854797e9cc7")
  (when (featurep! :checkers syntax)
    (package! flycheck-moonscript
      :recipe (:host github :repo "hlissner/emacs-flycheck-moonscript")
      :pin "fcb99e5efcf31db05f236f02eaa575986a57172d")))

(when (featurep! :completion company)
  (package! company-lua :pin "29f6819de4d691e5fd0b62893a9f4fbc1c6fcb52"))
