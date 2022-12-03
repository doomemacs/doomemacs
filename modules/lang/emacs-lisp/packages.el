;; -*- no-byte-compile: t; -*-
;;; lang/emacs-lisp/packages.el

(package! elisp-mode :built-in t)

;; Fontification plugins
(package! highlight-quoted :pin "24103478158cd19fbcfb4339a3f1fa1f054f1469")

;; Tools
(package! macrostep :pin "424e3734a1ee526a1bd7b5c3cd1d3ef19d184267")
(package! overseer :pin "02d49f582e80e36b4334c9187801c5ecfb027789")
(package! elisp-def :pin "dfca043ec0cbead67bd9c526cb009daf771d0fa2")
(package! elisp-demos :pin "01c301b516e9949d0239d20f6834afbc9acf0abb")
(when (modulep! :checkers syntax)
  (package! flycheck-package :pin "615c1ed8c6fb7c73abec6aaa73d3fef498d231bc")
  (package! flycheck-cask :pin "4b2ede6362ded4a45678dfbef1876faa42edbd58"))

;; Libraries
(package! buttercup :pin "ceedad5efa797e860dbb356bc2c3028a4e0321ec")
