;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode :pin "9de2df5a9f2f864c82ec112d3369154767a2bb49")
(package! markdown-toc :pin "d22633b654193bcab322ec51b6dd3bb98dd5f69f")

;; Required by `markdown-mode', or it will install it via package.el if it isn't
;; present when you call `markdown-edit-code-block'.
(package! edit-indirect :pin "82a28d8a85277cfe453af464603ea330eae41c05")

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! markdown-ts-mode
    :built-in 'prefer  ; Emacs 31+ has a superior markdown-ts-mode
    :pin "2f1ee8b94cdf53cebc31ae08ecfbba846193d5e1"))

(when (modulep! +grip)
  (package! grip-mode :pin "b8b9e603edbb258ab38a94a0518c4a8c7a22e53c"))

(when (modulep! :editor evil +everywhere)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown")
    :pin "8e6cc68af83914b2fa9fd3a3b8472573dbcef477"))
