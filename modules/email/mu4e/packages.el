;; -*- no-byte-compile: t; -*-
;;; email/mu4e/packages.el

(when (modulep! +org)
  (package! org-msg :pin "055de4abf611c5d5e12c770fe149c1861b402817"))

(package! mu4e-alert :pin "6beda20fc69771f2778f507c4a9e069dbaf1b628")
