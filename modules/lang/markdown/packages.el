;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode :pin "e096bb97a91fcd4dc2b46d8b6e093194b03b7364")
(package! markdown-toc :pin "3d724e518a897343b5ede0b976d6fb46c46bcc01")
(package! edit-indirect :pin "82a28d8a85277cfe453af464603ea330eae41c05")

(when (modulep! +grip)
  (package! grip-mode :pin "e145adb22593a88249d964f77174207bcf755493"))

(when (modulep! :editor evil +everywhere)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown")
    :pin "8e6cc68af83914b2fa9fd3a3b8472573dbcef477"))
