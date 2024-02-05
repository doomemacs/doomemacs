;; -*- no-byte-compile: t; -*-
;;; lang/dart/packages.el

(package! dart-mode :pin "dffc0209a19fdfac72b861d6adb445c1b6b464f7")

(when (and (modulep! +lsp)
           (not (modulep! :tools lsp +eglot)))
  (package! lsp-dart :pin "e7ee6afc2e165291360fd35d16648307920837c7"))

(when (modulep! +flutter)
  (package! flutter :pin "004c91e070a9b4a2a5042f5bb20015ec65453acf")
  (package! hover :pin "4ca0638a14a8b304ac2b46e7b342b8d8732ad199"))
