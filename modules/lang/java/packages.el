;; -*- no-byte-compile: t; -*-
;;; lang/java/packages.el

(package! android-mode :pin "d5332e339a")
(package! groovy-mode :pin "cafdd98e06")

(when (featurep! +meghanada)
  (package! meghanada :pin "70bfbf553c"))

(when (featurep! +eclim)
  (package! eclim :pin "23f5b294f8")
  (when (featurep! :completion company)
    (package! company-emacs-eclim :pin "23f5b294f8")))

(when (featurep! +lsp)
  (package! lsp-java :pin "fe118246903b9cfa9217f442f2b1b6524916253b"))
