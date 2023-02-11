;; -*- no-byte-compile: t; -*-
;;; ui/modeline/packages.el

(unless (modulep! +light)
  (package! doom-modeline :pin "b66d5e5006df4cedb1119da3d83fd6c08965b830")
  (package! compat :pin "2bedcb5ea91914e75d4905bc53e537b33f8f51e9"))
(package! anzu :pin "5abb37455ea44fa401d5f4c1bdc58adb2448db67")
(when (modulep! :editor evil)
  (package! evil-anzu :pin "d1e98ee6976437164627542909a25c6946497899"))
