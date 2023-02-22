;; -*- no-byte-compile: t; -*-
;;; lang/web/packages.el

;; +html.el
(package! emmet-mode :pin "63b6932603184956b5ea8919036d2b307b48d7fd")
(package! haml-mode :pin "bf5b6c11b1206759d2b28af48765e04882dd1fc4")
(package! pug-mode :pin "73f8c2f95eba695f701df20c8436f49abadebdc1")
(package! slim-mode :pin "3636d18ab1c8b316eea71c4732eb44743e2ded87")
(when (package! web-mode :pin "df97ab62c94826a84adbde060a4111ca91deda8e")
  (when (modulep! :completion company)
    (package! company-web :pin "863fb84b81ed283474e50330cd8d27b1ca0d74f1")))

;; +css.el
(package! css-mode :built-in t)
(package! less-css-mode :built-in t :pin "c7fa3d56d83206b28657f2e56439dc62280a2bf2")

(package! sass-mode :pin "247a0d4b509f10b28e4687cd8763492bca03599b")
(package! stylus-mode :pin "1ad7c51f3c6a6ae64550d9510c5e4e8470014375")
(package! sws-mode :pin "1ad7c51f3c6a6ae64550d9510c5e4e8470014375")
(package! rainbow-mode :pin "8e96388fb4d616a9dde23e712bad0d9cd048fbf0")
(when (modulep! :completion ivy)
  (package! counsel-css :pin "8e9c0515fc952452eee786d8ebb43d48ea86c9f8"))
(when (modulep! :completion helm)
  (package! helm-css-scss :pin "48b996f73af1fef8d6e88a1c545d98f8c50b0cf3"))
