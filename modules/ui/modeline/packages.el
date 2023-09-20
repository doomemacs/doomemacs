;; -*- no-byte-compile: t; -*-
;;; ui/modeline/packages.el

(unless (modulep! +light)
  (package! doom-modeline :pin "93f240f7a0bf35511cfc0a8dd75786744b4bcf77"))
(package! anzu :pin "5abb37455ea44fa401d5f4c1bdc58adb2448db67")
(when (modulep! :editor evil)
  (package! evil-anzu :pin "d1e98ee6976437164627542909a25c6946497899"))
