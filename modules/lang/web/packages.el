;; -*- no-byte-compile: t; -*-
;;; lang/web/packages.el

;; +html.el
(package! emmet-mode :pin "322d3bb112fced57d63b44863357f7a0b7eee1e3")
(package! haml-mode :pin "a64d58df8f098f858c6c11fa1629a90969f9c7e8")
(package! pug-mode :pin "73f8c2f95eba695f701df20c8436f49abadebdc1")
(package! slim-mode :pin "8c92169817f2fa59255f547f0a9fb4fbb8309db9")
(when (package! web-mode :pin "be2d59c8fa02b1a45ae54ce4079e502e659cefe6")
  (when (modulep! :completion company)
    (package! company-web :pin "863fb84b81ed283474e50330cd8d27b1ca0d74f1")))

;; +css.el
(package! css-mode :built-in t)
(package! less-css-mode :built-in t :pin "c78b88ff9af245daf05d45f5adf4befc11c1d4ef")

(package! sass-mode :pin "247a0d4b509f10b28e4687cd8763492bca03599b")
(package! stylus-mode :pin "1ad7c51f3c6a6ae64550d9510c5e4e8470014375")
(package! sws-mode :pin "1ad7c51f3c6a6ae64550d9510c5e4e8470014375")
(package! rainbow-mode :pin "2e6b18609c2fdd1a2dc513937a64d276fd6cf24c")
(when (modulep! :completion ivy)
  (package! counsel-css :pin "8e9c0515fc952452eee786d8ebb43d48ea86c9f8"))
(when (modulep! :completion helm)
  (package! helm-css-scss :pin "2169d83d8fdc661241df208cb3235112735d936e"))
