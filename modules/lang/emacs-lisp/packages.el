;; -*- no-byte-compile: t; -*-
;;; lang/emacs-lisp/packages.el

(package! elisp-mode :built-in t)

(package! highlight-quoted :pin "24103478158cd19fbcfb4339a3f1fa1f054f1469")
(package! macrostep :pin "424e3734a1ee526a1bd7b5c3cd1d3ef19d184267")
(package! overseer :pin "02d49f582e80e36b4334c9187801c5ecfb027789")
(package! elisp-def :pin "368b04da68783601b52e3169312183381871cf9e")
(package! elisp-demos :pin "bec206bf1b2ccc899120ec4ca2fcdcf30dcf0da8")

(when (featurep! :checkers syntax)
  (package! flycheck-cask :pin "3457ae553c4feaf8168008f063d78fdde8fb5f94"))

(package! buttercup :pin "b297b1dbfa21c87ffbcfc12d19262765387848de")
