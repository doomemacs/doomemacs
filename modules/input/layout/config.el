;;; input/keymaps/config.el -*- lexical-binding: t; -*-

(defvar doom-bepo-cr-rotation-style 'ergodis
  "Modify this variable in your $DOOMDIR/init.el
Style of binding rotation for the cr keys.
If 'ergodis, then the module maps the old 'c' bindings to 'l' and the old 'r' to 'h', as
the 'change' function is used more often and 'l' is easier to reach than 'h' in bépo.

If 'strict, the module does a normal swap and 'c' bindings go to 'h', 'r' bindings go to 'l'.

In all cases, 'h' functions go to 'c' and 'l' ones go to 'r' so the navigation keys still feel vim-like.")

(when (featurep! +bepo)
  (load! "+bepo"))
