;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode :pin "182640f79c3ed66f82f0419f130dffc173ee9464")
(package! markdown-toc :pin "d22633b654193bcab322ec51b6dd3bb98dd5f69f")

;; Required by `markdown-mode', or it will install it via package.el if it isn't
;; present when you call `markdown-edit-code-block'.
(package! edit-indirect :pin "82a28d8a85277cfe453af464603ea330eae41c05")

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! markdown-ts-mode
    :built-in 'prefer  ; Emacs 31+ has a superior markdown-ts-mode
    :pin "2f1ee8b94cdf53cebc31ae08ecfbba846193d5e1"))

(when (modulep! +grip)
  (package! grip-mode :pin "d2d27240d0150c00f0b9a5d7d840357e84d4728d"))

(when (modulep! :editor evil +everywhere)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown")
    :pin "8e6cc68af83914b2fa9fd3a3b8472573dbcef477"))
