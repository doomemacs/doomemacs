;; -*- no-byte-compile: t; -*-
;;; ui/ophints/packages.el

(if (featurep! :editor evil)
    (package! evil-goggles :pin "08a22058fd6a167f9f1b684c649008caef571459")
  (package! volatile-highlights :pin "9a20091f0ce7fc0a6b3e641a6a46d5f3ac4d8392"))
