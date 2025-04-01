;; -*- no-byte-compile: t; -*-
;;; input/chinese/packages.el

(package! pyim :pin "64731c213cf50f44f44f9a7d1ab269b387e66663")
(package! fcitx :pin "b399482ed8db5893db2701df01db4c38cccda495")
(package! ace-pinyin :pin "47662c0b05775ba353464b44c0f1a037c85e746e")
(package! pangu-spacing :pin "6509df9c90bbdb9321a756f7ea15bb2b60ed2530")
(when (modulep! +rime)
  (package! liberime :pin "23c0caa1bf73f4e9ab58d52dc46cf21088dc6c54"))
(when (modulep! +childframe)
  (package! posframe :pin "12f540c9ad5da09673b2bca1132b41f94c134e82"))
(when (modulep! :editor evil +everywhere)
  (package! evil-pinyin
    :recipe (:build (:not autoloads))
    :pin "0fae5ad8761417f027b33230382a50f826ad3bfb"))
