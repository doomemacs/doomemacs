;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode :pin "90ad4af79a8bb65a3a5cdd6314be44abd9517cfc")
(package! markdown-toc :pin "1b0c5ae7f306f60b909390cad009b76660dd5acd")

;; Required by `markdown-mode', or it will install it via package.el if it isn't
;; present when you call `markdown-edit-code-block'.
(package! edit-indirect :pin "82a28d8a85277cfe453af464603ea330eae41c05")

(when (modulep! +tree-sitter)
  (package! markdown-ts-mode
    :built-in 'prefer  ; Emacs 31+ has a superior markdown-ts-mode
    :pin "2f1ee8b94cdf53cebc31ae08ecfbba846193d5e1"))

(when (modulep! +grip)
  (package! grip-mode :pin "96a927dce69d7607b981d7754cf8b415ebf9d6a8"))

(when (modulep! :editor evil +everywhere)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown")
    :pin "8e6cc68af83914b2fa9fd3a3b8472573dbcef477"))
