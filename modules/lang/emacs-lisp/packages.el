;; -*- no-byte-compile: t; -*-
;;; lang/emacs-lisp/packages.el

(package! elisp-mode :built-in t)

;; Fontification plugins
(package! highlight-quoted :pin "24103478158cd19fbcfb4339a3f1fa1f054f1469")

;; Tools
(package! macrostep :pin "424e3734a1ee526a1bd7b5c3cd1d3ef19d184267")
(package! overseer :pin "02d49f582e80e36b4334c9187801c5ecfb027789")
(package! elisp-def :pin "dfca043ec0cbead67bd9c526cb009daf771d0fa2")
(package! elisp-demos :pin "924b07d28e4f5b82f0e1377bcde800068f0a6d9d")
(when (featurep! :checkers syntax)
  (package! flycheck-package :pin "ecd03f83790611888d693c684d719e033f69cb40")
  (package! flycheck-cask :pin "4b2ede6362ded4a45678dfbef1876faa42edbd58"))

;; Libraries
(package! buttercup :pin "1de6be465cfe2c3f00183de9351bd838690c9f81")
