;; -*- no-byte-compile: t; -*-
;;; lang/emacs-lisp/packages.el

(package! elisp-mode :built-in t)

;; Fontification plugins
(package! highlight-quoted :pin "24103478158cd19fbcfb4339a3f1fa1f054f1469")

;; Tools
(package! macrostep :pin "0b04a89f698c335c9ea492553470a8d45c113edd")
(package! overseer :pin "02d49f582e80e36b4334c9187801c5ecfb027789")
(package! elisp-def :pin "1d2e88a232ec16bce036b49577c4d4d96035f9f7")
(package! elisp-demos :pin "8d0cd806b109076e6c4383edf59dbab9435dc5dc")
(when (and (modulep! :checkers syntax)
           (not (modulep! :checkers syntax +flymake)))
  (package! flycheck-package :pin "3a6aaed29ff61418c48c0251e1432c30748ae739")
  (package! flycheck-cask :pin "4b2ede6362ded4a45678dfbef1876faa42edbd58"))

;; Libraries
(package! buttercup :pin "30c703d215b075aaede936a2c424f65b5f7b6391")
