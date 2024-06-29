;; -*- no-byte-compile: t; -*-
;;; input/chinese/packages.el

(package! pyim :pin "f22c20f2e6af55b3a758defabe4c842fb94cde2b")
(package! fcitx :pin "b399482ed8db5893db2701df01db4c38cccda495")
(package! ace-pinyin :pin "47662c0b05775ba353464b44c0f1a037c85e746e")
(package! pangu-spacing :pin "2303013e5cd7852136f1429162fea0e1c8cb0221")
(when (modulep! +rime)
  (package! liberime :pin "c5839f541763f661a4d46784f3f14adad28ee2b0"))
(when (modulep! +childframe)
  (package! posframe :pin "f4e9e509ba96ceb3c2b2b054957291607fb52651"))
(when (modulep! :editor evil +everywhere)
  (package! evil-pinyin
    :recipe (:build (:not autoloads))
    :pin "0fae5ad8761417f027b33230382a50f826ad3bfb"))
