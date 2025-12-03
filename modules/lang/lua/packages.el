;; -*- no-byte-compile: t; -*-
;;; lang/lua/packages.el

(package! lua-mode :pin "2f6b8d7a6317e42c953c5119b0119ddb337e0a5f")

(when (modulep! +moonscript)
  (package! moonscript :pin "56f90471e2ced2b0a177aed4d8c2f854797e9cc7")
  (when (modulep! :checkers syntax -flymake)
    (package! flycheck-moonscript
      :recipe (:host github :repo "hlissner/emacs-flycheck-moonscript")
      :pin "fcb99e5efcf31db05f236f02eaa575986a57172d")))

(when (modulep! +fennel)
  (package! fennel-mode :pin "c1bccdec9e8923247c9b1a5ffcf14039d2ddb227"))

(when (modulep! :completion company)
  (package! company-lua :pin "29f6819de4d691e5fd0b62893a9f4fbc1c6fcb52"))
