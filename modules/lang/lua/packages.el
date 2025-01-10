;; -*- no-byte-compile: t; -*-
;;; lang/lua/packages.el

(package! lua-mode :pin "d074e4134b1beae9ed4c9b512af741ca0d852ba3")

(when (modulep! +moonscript)
  (package! moonscript :pin "56f90471e2ced2b0a177aed4d8c2f854797e9cc7")
  (when (modulep! :checkers syntax -flymake)
    (package! flycheck-moonscript
      :recipe (:host github :repo "hlissner/emacs-flycheck-moonscript")
      :pin "fcb99e5efcf31db05f236f02eaa575986a57172d")))

(when (modulep! +fennel)
  (package! fennel-mode :pin "3632cc77de3ba350297b46bea9ccc3e41e4c1def"))

(when (modulep! :completion company)
  (package! company-lua :pin "29f6819de4d691e5fd0b62893a9f4fbc1c6fcb52"))
