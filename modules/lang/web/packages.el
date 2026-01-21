;; -*- no-byte-compile: t; -*-
;;; lang/web/packages.el

;; +html.el
(package! emmet-mode :pin "322d3bb112fced57d63b44863357f7a0b7eee1e3")
(package! haml-mode :pin "3bb4a96535eb5c81dbe6a43bfa8d67a778d449c0")
(package! pug-mode :pin "73f8c2f95eba695f701df20c8436f49abadebdc1")
(package! slim-mode :pin "0b1b3803290f749cb85084adc75013254b513d41")
(when (package! web-mode :pin "1e7694aee87722f9e51b6e39c35d175d83a1fb2c")
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
