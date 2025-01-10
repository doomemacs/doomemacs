;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode :pin "e1007785947ac8b32d8977a2843a11aded40682b")
(package! markdown-toc :pin "d2fb4cbd95e558042307d706f9f47f93687c9fcc")

;; Required by `markdown-mode', or it will install it via package.el if it isn't
;; present when you call `markdown-edit-code-block'.
(package! edit-indirect :pin "82a28d8a85277cfe453af464603ea330eae41c05")

(when (modulep! +grip)
  (package! grip-mode :pin "df0ba7589db7c54c4b68bd6a96a246676d7893c3"))

(when (modulep! :editor evil +everywhere)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown")
    :pin "8e6cc68af83914b2fa9fd3a3b8472573dbcef477"))
