;; -*- no-byte-compile: t; -*-
;;; email/mu4e/packages.el

(when (modulep! +org)
  (package! org-msg :pin "0b65f0f77a7a71881ddfce19a8cdc60465bda057"))
