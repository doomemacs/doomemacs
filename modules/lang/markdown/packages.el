;; -*- no-byte-compile: t; -*-
;;; lang/markdown/packages.el

(package! markdown-mode :pin "92802fae9ebbc8c2e4c281c06dcdbd74b8bca80e")
(package! markdown-toc :pin "d3324cb6bc1001b1c820f04cc0e45e99ee16a0c8")

;; Required by `markdown-mode', or it will install it via package.el if it isn't
;; present when you call `markdown-edit-code-block'.
(package! edit-indirect :pin "82a28d8a85277cfe453af464603ea330eae41c05")

(when (and (modulep! +tree-sitter) (treesit-available-p))
  (package! markdown-ts-mode
    :built-in 'prefer  ; Emacs 31+ has a superior markdown-ts-mode
    :pin "2f1ee8b94cdf53cebc31ae08ecfbba846193d5e1"))

(when (modulep! +grip)
  (package! grip-mode :pin "26bdadf604b34e5a6b9628f3476bf7f5e88d2c3d"))

(when (modulep! :editor evil +everywhere)
  (package! evil-markdown
    :recipe (:host github :repo "Somelauw/evil-markdown")
    :pin "8e6cc68af83914b2fa9fd3a3b8472573dbcef477"))
