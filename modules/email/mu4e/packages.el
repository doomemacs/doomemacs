;; -*- no-byte-compile: t; -*-
;;; email/mu4e/packages.el

(when (modulep! +org)
  (package! org-msg :pin "0b65f0f77a7a71881ddfce19a8cdc60465bda057"))

(package! mu4e-alert :pin "6beda20fc69771f2778f507c4a9e069dbaf1b628")
