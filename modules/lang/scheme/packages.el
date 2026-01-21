;; -*- no-byte-compile: t; -*-
;;; lang/scheme/packages.el

(when (< emacs-major-version 29)
  (package! scheme
    :recipe (:host gitlab :repo "flatwhatson/scheme-mode")
    :pin "a713b253d2ff5bbaeef400a7d495f7dfe918a641"))

(when (package! geiser :pin "8842104d1521a00c182ce78e9d50d394e9ba86f5")
  (package! macrostep-geiser :pin "f6a2d5bb96ade4f23df557649af87ebd0cc45125")
  (when (modulep! +chez)
    (package! geiser-chez :pin "605a81ff7b2d2b275a3ec68e3ce7e5b50f85014d"))
  (when (modulep! +chibi)
    (package! geiser-chibi :pin "2502fed1349c2703eea528b74bcc980ad6bceab8"))
  (when (modulep! +chicken)
    (package! geiser-chicken :pin "8342bad8ce1c79fee3563cd95ee27a8b062f6a9e"))
  (when (modulep! +gambit)
    (package! geiser-gambit :pin "381d74ca5059b44fe3d8b5daf42214019c6d1a88"))
  (when (modulep! +gauche)
    (package! geiser-gauche :pin "b8197c6288020371d2a27ca734940564876d5d98"))
  (when (modulep! +guile)
    (package! geiser-guile :pin "a0f111f8dedd31c593c4ed12c0b99745f3c1340f")
    (when (modulep! :checkers syntax -flymake)
      (package! flycheck-guile
        :recipe (:host github :repo "flatwhatson/flycheck-guile")
        :pin "dd7bbdc48fd21cf8d270c913c56cd580f8ec3d03")))
  (when (modulep! +kawa)
    (package! geiser-kawa :pin "5896b19642923f74f718eb68d447560b2d26d797"))
  (when (modulep! +mit)
    (package! geiser-mit :pin "ddd2ba733e8274d40a26b5d6d2ee11f1bac8abe6"))
  (when (modulep! +racket)
    (package! geiser-racket :pin "22e56ce80389544d3872cf4beb4008fb514b2218")))
