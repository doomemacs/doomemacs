;; -*- no-byte-compile: t; -*-
;;; lang/lua/packages.el

(package! lua-mode :pin "1f596a93b3")

(when (featurep! +moonscript)
  (package! moonscript :pin "56f90471e2")
  (when (featurep! :checkers syntax)
    (package! flycheck-moonscript
      :recipe (:host github :repo "hlissner/emacs-flycheck-moonscript") :pin "fcb99e5efc")))

(when (featurep! :completion company)
  (package! company-lua :pin "29f6819de4"))
