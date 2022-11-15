;; -*- no-byte-compile: t; -*-
;;; lang/scheme/packages.el

(when (< emacs-major-version 29)
  (package! scheme
    :recipe (:host gitlab :repo "flatwhatson/scheme-mode")
    :pin "aaef1f88cc34e8b6e07c207f9b8caff33f6e0740"))

(when (package! geiser :pin "e54d5e6dc659c252d10c4280f4c4d78d38623df5")
  (package! macrostep-geiser :pin "f6a2d5bb96ade4f23df557649af87ebd0cc45125")
  (when (modulep! +chez)
    (package! geiser-chez :pin "246ec4c8bc4e7f64414e5cbe0fa66f0e5ef7d527"))
  (when (modulep! +chibi)
    (package! geiser-chibi :pin "5a6a5a580ea45cd4974df21629a8d50cbe3d6e99"))
  (when (modulep! +chicken)
    (package! geiser-chicken :pin "a480598b5908c95bc8d3178a48f13e9072a9235b"))
  (when (modulep! +gambit)
    (package! geiser-gambit :pin "381d74ca5059b44fe3d8b5daf42214019c6d1a88"))
  (when (modulep! +gauche)
    (package! geiser-gauche :pin "8ff743f6416f00751e24aef8b9791501a40f5421"))
  (when (modulep! +guile)
    (package! geiser-guile :pin "24ce15de235c105daf5ecfb818200dae1c9815ee")
    (when (modulep! :checkers syntax)
      (package! flycheck-guile
        :recipe (:host github :repo "flatwhatson/flycheck-guile")
        :pin "e23a4d7813179124fd98abf1c2f4190a72569bee")))
  (when (modulep! +kawa)
    (package! geiser-kawa :pin "5896b19642923f74f718eb68d447560b2d26d797"))
  (when (modulep! +mit)
    (package! geiser-mit :pin "4e90e9ae815e89f3540fb9644e6016c663ef5765"))
  (when (modulep! +racket)
    (package! geiser-racket :pin "22e56ce80389544d3872cf4beb4008fb514b2218")))
