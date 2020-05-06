;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company :pin "6333fc4ebbbf4d28e834de8715561e984f149ecb")
(package! company-dict :pin "cd7b8394f6014c57897f65d335d6b2bd65dab1f4")
(package! company-prescient :pin "0f4a89bdec61395138d968a38d375e63ccfbed63")
(when (featurep! +childframe)
  (package! company-box :pin "3814fcb14e92f4b85b19e664e216a7c8d5c7144d"))
