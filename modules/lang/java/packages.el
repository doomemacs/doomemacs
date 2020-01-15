;; -*- no-byte-compile: t; -*-
;;; lang/java/packages.el

(package! android-mode :pin "d5332e339a1f5e30559a53feffb8442ca79265d6")
(package! groovy-mode :pin "cafdd98e06a3bbff213f3ccb163de2c42d412b66")

(when (featurep! +meghanada)
  (package! meghanada :pin "70bfbf553c7b7fb1928672e9a95b7137e02c2d4b"))

(when (featurep! +eclim)
  (package! eclim :pin "23f5b294f833ce58516d7b9ae08a7792d70022a1")
  (when (featurep! :completion company)
    (package! company-emacs-eclim :pin "23f5b294f833ce58516d7b9ae08a7792d70022a1")))

(when (featurep! +lsp)
  (package! lsp-java :pin "5c3da6cf3a27dc57df1d537bdacbb318ca628bcf"))
