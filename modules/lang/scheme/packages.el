;; -*- no-byte-compile: t; -*-
;;; lang/scheme/packages.el

(when (< emacs-major-version 29)
  (package! scheme
    :recipe (:host gitlab :repo "flatwhatson/scheme-mode")
    :pin "51e586e5f1ddb5ea71d2cac8d401faf718c4627e"))

(when (package! geiser :pin "49d12a14482ce9a193c049a5ccdd2fed90b0bdbe")
  (package! macrostep-geiser :pin "f6a2d5bb96ade4f23df557649af87ebd0cc45125")
  (when (modulep! +chez)
    (package! geiser-chez :pin "605a81ff7b2d2b275a3ec68e3ce7e5b50f85014d"))
  (when (modulep! +chibi)
    (package! geiser-chibi :pin "5a6a5a580ea45cd4974df21629a8d50cbe3d6e99"))
  (when (modulep! +chicken)
    (package! geiser-chicken :pin "a480598b5908c95bc8d3178a48f13e9072a9235b"))
  (when (modulep! +gambit)
    (package! geiser-gambit :pin "381d74ca5059b44fe3d8b5daf42214019c6d1a88"))
  (when (modulep! +gauche)
    (package! geiser-gauche :pin "8ff743f6416f00751e24aef8b9791501a40f5421"))
  (when (modulep! +guile)
    (package! geiser-guile :pin "ee33af6b4ef8fdaccbdf93c62b50fe17c07a1ade")
    (when (and (modulep! :checkers syntax)
             (not (modulep! :checkers syntax +flymake)))
      (package! flycheck-guile
        :recipe (:host github :repo "flatwhatson/flycheck-guile")
        :pin "dd7bbdc48fd21cf8d270c913c56cd580f8ec3d03")))
  (when (modulep! +kawa)
    (package! geiser-kawa :pin "5896b19642923f74f718eb68d447560b2d26d797"))
  (when (modulep! +mit)
    (package! geiser-mit :pin "4e90e9ae815e89f3540fb9644e6016c663ef5765"))
  (when (modulep! +racket)
    (package! geiser-racket :pin "22e56ce80389544d3872cf4beb4008fb514b2218")))
