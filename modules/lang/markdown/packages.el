;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode :pin "d2d960bec164e91e1a3315b176e2bcc324e63c95")
(package! markdown-toc :pin "d2fb4cbd95e558042307d706f9f47f93687c9fcc")

;; Required by `markdown-mode', or it will install it via package.el if it isn't
;; present when you call `markdown-edit-code-block'.
(package! edit-indirect :pin "82a28d8a85277cfe453af464603ea330eae41c05")

(when (modulep! +grip)
  (package! grip-mode :pin "e90e3b47d8fcbb7625106e1ea840519a58c2c39c"))

(when (modulep! :editor evil +everywhere)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown")
    :pin "8e6cc68af83914b2fa9fd3a3b8472573dbcef477"))
