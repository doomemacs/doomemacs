;; -*- no-byte-compile: t; -*-
;;; input/chinese/packages.el

(package! pyim :pin "64067b20ce0e964b1342b378180f24a1d4503797")
(package! fcitx :pin "b399482ed8db5893db2701df01db4c38cccda495")
(package! ace-pinyin :pin "47662c0b05775ba353464b44c0f1a037c85e746e")
(package! pangu-spacing :pin "2303013e5cd7852136f1429162fea0e1c8cb0221")
(when (modulep! +rime)
  (package! liberime :pin "cc9eb9812fd6f68e78ed6a0c0a85da7a18765753"))
(when (modulep! +childframe)
  (package! posframe :pin "017deece88360c7297265680d78a0bb316470716"))
(when (modulep! :editor evil +everywhere)
  (package! evil-pinyin
    :recipe (:build (:not autoloads))
    :pin "0fae5ad8761417f027b33230382a50f826ad3bfb"))
