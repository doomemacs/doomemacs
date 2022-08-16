;; -*- no-byte-compile: t; -*-
;;; input/chinese/packages.el

(package! pyim :pin "02c50045cb14ab253d8d8435e83e7f10b0bbc130")
(package! fcitx :pin "12dc2638ddd15c8f6cfaecb20e1f428ab2bb5624")
(package! ace-pinyin :pin "47662c0b05775ba353464b44c0f1a037c85e746e")
(package! pangu-spacing :pin "f92898949ba3bf991fd229416f3bbb54e9c6c223")
(when (modulep! +rime)
  (package! liberime :pin "8291e22cd0990a99cb2f88ca67a9065a157f39af"))
(when (modulep! +childframe)
  (package! posframe :pin "0d23bc5f7cfac00277d83ae7ba52c48685bcbc68"))
