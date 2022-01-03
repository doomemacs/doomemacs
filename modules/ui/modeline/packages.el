;; -*- no-byte-compile: t; -*-
;;; ui/modeline/packages.el

(unless (featurep! +light)
  (package! doom-modeline :pin "84573ae5e7db2705da67c2eda94b786a99346272"))
(package! anzu :pin "5abb37455ea44fa401d5f4c1bdc58adb2448db67")
(when (featurep! :editor evil)
  (package! evil-anzu :pin "d3f6ed4773b48767bd5f4708c7f083336a8a8a86"))
