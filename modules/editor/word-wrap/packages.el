;; -*- no-byte-compile: t; -*-
;;; editor/word-wrap/packages.el

(package! adaptive-wrap :pin "70005d2012ab57c20be03c05aebd49318fe49c99")
(package! visual-fill-column
  :recipe (:host github :repo "emacsmirror/visual-fill-column")
  :pin "5e74afe39d0afb911dae51af4e7a60ccdf9701f3")
