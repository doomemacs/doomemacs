;; -*- no-byte-compile: t; -*-
;;; lang/lua/packages.el

(package! lua-mode :pin "ad639c62e38a110d8d822c4f914af3e20b40ccc4")

(when (modulep! +moonscript)
  (package! moonscript :pin "56f90471e2ced2b0a177aed4d8c2f854797e9cc7")
  (when (modulep! :checkers syntax)
    (package! flycheck-moonscript
      :recipe (:host github :repo "hlissner/emacs-flycheck-moonscript")
      :pin "fcb99e5efcf31db05f236f02eaa575986a57172d")))

(when (modulep! +fennel)
  (package! fennel-mode :pin "52da9715b0d046597cb322d3a98cb27ae6e5719e"))

(when (modulep! :completion company)
  (package! company-lua :pin "29f6819de4d691e5fd0b62893a9f4fbc1c6fcb52"))
