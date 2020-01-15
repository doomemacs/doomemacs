;; -*- no-byte-compile: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg :pin "c12061eb80c1487a1963af7cdae268d709a70ca9")

(unless (featurep! +lsp)
  (package! merlin :pin "f6954e953b4168e6a798a0255d6a2dfbd868a3c6")
  (package! merlin-eldoc :pin "db7fab1eddfe34781b7e79694f8923b285698032")
  (when (featurep! :checkers syntax)
    (package! flycheck-ocaml :pin "8707a7bf545a8639a6a5c600a98d9a2ea1487dc9")))

(package! ocp-indent :pin "9e26c0a2699b7076cebc04ece59fb354eb84c11c")

(when (featurep! :tools eval)
  (package! utop :pin "7c99d8c904dbb6fb0daf375f5424a9f6053b9c84"))

(when (featurep! :editor format)
  ;; by default quelpa generated a version 0pre0.20180929.192844, which got
  ;; parsed into (0 -1 0 ...), which when compared with version nil (0) in
  ;; package-installed-p always yielded false
  (package! ocamlformat :recipe
    (:host github :repo "ocaml-ppx/ocamlformat" :files ("emacs/*.el")) :pin "c7376847027ec94929fb3e3c42ba76d03c952d6d"))

(package! dune :recipe
  (:host github :repo "ocaml/dune" :files ("editor-integration/emacs/*.el")) :pin "19f3a4a3db93702034c5227517b3e96e89fe5d84")
