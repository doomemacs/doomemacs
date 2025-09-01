;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode :pin "d51c469133d220823cc6ab50ff8e8743ed6e42fb")
(package! markdown-toc :pin "e3429d3014655b7d2ff6d5ac12b27d4d94fdcee4")

;; Required by `markdown-mode', or it will install it via package.el if it isn't
;; present when you call `markdown-edit-code-block'.
(package! edit-indirect :pin "82a28d8a85277cfe453af464603ea330eae41c05")

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! markdown-ts-mode
    :built-in 'prefer  ; Emacs 31+ has a superior markdown-ts-mode
    :pin "2f1ee8b94cdf53cebc31ae08ecfbba846193d5e1"))

(when (modulep! +grip)
  (package! grip-mode :pin "11fecd5b38c78597ff53a39fb3a090e7c80350fa"))

(when (modulep! :editor evil +everywhere)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown")
    :pin "8e6cc68af83914b2fa9fd3a3b8472573dbcef477"))
