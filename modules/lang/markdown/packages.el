;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode :pin "b8637bae075231d70fe7f845305eaba2c0240d89")
(package! markdown-toc :pin "3d724e518a897343b5ede0b976d6fb46c46bcc01")

;; Required by `markdown-mode', or it will install it via package.el if it isn't
;; present when you call `markdown-edit-code-block'.
(package! edit-indirect :pin "82a28d8a85277cfe453af464603ea330eae41c05")

(when (modulep! +grip)
  (package! grip-mode :pin "d6c7e33e40d10b529b059ea1d237161dc3e88428"))

(when (modulep! :editor evil +everywhere)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown")
    :pin "8e6cc68af83914b2fa9fd3a3b8472573dbcef477"))
