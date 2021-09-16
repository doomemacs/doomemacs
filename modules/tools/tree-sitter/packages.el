;; -*- no-byte-compile: t; -*-
;;; tools/tree-sitter/packages.el

(package! tree-sitter
  :pin "4d9871d23999fe5f8de821e23c9ec576df2b2738")

(package! tree-sitter-langs
  :pin "fa47b55f7bd11bd2b17ab48deb03ed23000bb974")

(when (featurep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "eedc1f54611e4403ea228b33056388a8539a2b3e"))
