(package! odin-mode
  :recipe (:host github :repo "mattt-b/odin-mode"))

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! odin-ts-mode
    :recipe (:host github :repo "Sampie159/odin-ts-mode")))
