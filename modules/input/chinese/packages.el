;; -*- no-byte-compile: t; -*-
;;; input/chinese/packages.el

(package! pyim :pin "de7eff2a58d88b168e35c3c81484ea874991391c")
(package! fcitx :pin "12dc2638ddd15c8f6cfaecb20e1f428ab2bb5624")
(package! ace-pinyin :pin "47662c0b05775ba353464b44c0f1a037c85e746e")
(package! pangu-spacing :pin "2303013e5cd7852136f1429162fea0e1c8cb0221")
(when (modulep! +rime)
  (package! liberime :pin "8291e22cd0990a99cb2f88ca67a9065a157f39af"))
(when (modulep! +childframe)
  (package! posframe :pin "0d23bc5f7cfac00277d83ae7ba52c48685bcbc68"))
(when (modulep! :editor evil +everywhere)
  (package! evil-pinyin
    :recipe (:build (:not autoloads))
    :pin "3e9e501ded86f88e01a4edec5d526ab0fab879d7"))
