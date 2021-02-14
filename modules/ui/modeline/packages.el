;; -*- no-byte-compile: t; -*-
;;; ui/modeline/packages.el

(unless (featurep! +light)
  (package! doom-modeline :pin "116c733fd580f729e275d3b6b3954667d5dfdd5a"))
(package! anzu :pin "bdb3da5028935a4aea55c40769bc191a81afb54e")
(when (featurep! :editor evil)
  (package! evil-anzu :pin "d3f6ed4773b48767bd5f4708c7f083336a8a8a86"))
