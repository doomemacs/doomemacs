;; -*- no-byte-compile: t; -*-
;;; lang/web/packages.el

;; +html.el
(package! emmet-mode :pin "6b2e554f7fd27f732810f4b14ea01e3c54b7b3da")
(package! haml-mode :pin "bf5b6c11b1206759d2b28af48765e04882dd1fc4")
(package! pug-mode :pin "d08090485eb8c0488a7d2fbf63680dc0880c7d2f")
(package! slim-mode :pin "3636d18ab1c8b316eea71c4732eb44743e2ded87")
(when (package! web-mode :pin "61f057a6baeba6b3595e412ba79b3080dca17dcc")
  (when (featurep! :completion company)
    (package! company-web :pin "f0cc9187c9c34f72ad71f5649a69c74f996bae9a")))

;; +css.el
(package! css-mode :built-in t)
(package! less-css-mode :built-in t :pin "c7fa3d56d83206b28657f2e56439dc62280a2bf2")

(package! sass-mode :pin "247a0d4b509f10b28e4687cd8763492bca03599b")
(package! stylus-mode :pin "111460b056838854e470a6383041a99f843b93ee")
(package! sws-mode :pin "111460b056838854e470a6383041a99f843b93ee")
(package! rainbow-mode :pin "949166cc0146bc9fabf74ce70c1c4a097f4cffd4")
(when (featurep! :completion ivy)
  (package! counsel-css :pin "f7647b4195b9b4e97f1ee1acede6054ae38df630"))
(when (featurep! :completion helm)
  (package! helm-css-scss :pin "48b996f73af1fef8d6e88a1c545d98f8c50b0cf3"))
