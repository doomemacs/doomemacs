;; -*- no-byte-compile: t; -*-
;;; email/mu4e/packages.el

(when (modulep! +org)
  (package! org-msg :pin "7b1dfb96d558f6e5626e96d4f4a5150d55cb7eb2"))

(package! mu4e-alert :pin "3c9af8c7994df0a1a4f0703552ea3beffb485ace")
