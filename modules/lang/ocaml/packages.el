;; -*- no-byte-compile: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg)

(unless (featurep! +lsp)
  (package! merlin)
  (package! merlin-eldoc)
  (when (featurep! :tools flycheck)
    (package! flycheck-ocaml)))

(package! ocp-indent)

(when (featurep! :tools eval)
  (package! utop))

(when (featurep! :editor format)
  ;; by default quelpa generated a version 0pre0.20180929.192844, which got
  ;; parsed into (0 -1 0 ...), which when compared with version nil (0) in
  ;; package-installed-p always yielded false
  (package! ocamlformat :recipe
    (:host github :repo "ocaml-ppx/ocamlformat" :files ("emacs/*.el"))))

(package! dune :recipe
  (:host github :repo "ocaml/dune" :files ("editor-integration/emacs/*.el")))
