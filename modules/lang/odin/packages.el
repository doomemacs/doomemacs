;; -*- no-byte-compile: t; -*-
;;; lang/odin/packages.el

(package! odin-mode
  :recipe (:host github :repo "mattt-b/odin-mode")
  :pin "21c6ff8b49f5eaa2d3b9969feeb08de921f11e92")

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! odin-ts-mode
    :recipe (:host github :repo "Sampie159/odin-ts-mode")
    :pin "800134c4f104ab48b28ed33c8ebce1c8b8707add"))
