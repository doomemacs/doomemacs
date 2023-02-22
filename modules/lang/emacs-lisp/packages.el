;; -*- no-byte-compile: t; -*-
;;; lang/emacs-lisp/packages.el

(package! elisp-mode :built-in t)

;; Fontification plugins
(package! highlight-quoted :pin "24103478158cd19fbcfb4339a3f1fa1f054f1469")

;; Tools
(package! macrostep :pin "75ecd041219239f0dceab4883594ea2fe366b484")
(package! overseer :pin "02d49f582e80e36b4334c9187801c5ecfb027789")
(package! elisp-def :pin "1d2e88a232ec16bce036b49577c4d4d96035f9f7")
(package! elisp-demos :pin "792be709c82101aea0585ece7429e2fdded74494")
(when (modulep! :checkers syntax)
  (package! flycheck-package :pin "3a6aaed29ff61418c48c0251e1432c30748ae739")
  (package! flycheck-cask :pin "4b2ede6362ded4a45678dfbef1876faa42edbd58"))

;; Libraries
(package! buttercup :pin "07a52c99695845a0089e828d43da154c0ba0c178")
