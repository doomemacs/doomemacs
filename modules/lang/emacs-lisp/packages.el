;; -*- no-byte-compile: t; -*-
;;; lang/emacs-lisp/packages.el

(package! elisp-mode :built-in t)

;; Fontification plugins
(package! highlight-quoted :pin "24103478158cd19fbcfb4339a3f1fa1f054f1469")

;; Tools
(package! macrostep :pin "424e3734a1ee526a1bd7b5c3cd1d3ef19d184267")
(package! overseer :pin "02d49f582e80e36b4334c9187801c5ecfb027789")
(package! elisp-def :pin "368b04da68783601b52e3169312183381871cf9e")
(package! elisp-demos :pin "4cd55a30d5dbd8d36a0e6f87261c4fef17fc6db0")
(when (featurep! :checkers syntax)
  (package! flycheck-cask :pin "3457ae553c4feaf8168008f063d78fdde8fb5f94"))

;; Libraries
(package! buttercup :pin "532d082a363add4f6d9838ca3f924a773ce12136")
