;; -*- no-byte-compile: t; -*-
;;; lang/web/packages.el

;; +html.el
(package! emmet-mode :pin "1acb821e01")
(package! haml-mode :pin "bf5b6c11b1")
(package! pug-mode :pin "685fd3414d")
(package! slim-mode :pin "3636d18ab1")
(when (package! web-mode :pin "b0bb4ab82b")
  (when (featurep! :completion company)
    (package! company-web :pin "f0cc9187c9")))

;; +css.el
(package! css-mode :built-in t)
(package! less-css-mode :built-in t :pin "c7fa3d56d8")

(package! sass-mode :pin "247a0d4b50")
(package! stylus-mode :pin "4dbde92542")
(package! sws-mode :pin "4dbde92542")
(package! rainbow-mode :pin "3ef813d637")
(when (featurep! :completion ivy)
  (package! counsel-css :pin "61a38c9d50"))
(when (featurep! :completion helm)
  (package! helm-css-scss :pin "48b996f73a"))
