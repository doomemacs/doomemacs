;; -*- no-byte-compile: t; -*-
;;; email/notmuch/packages.el

(package! notmuch :pin "aba7fb375b")
(package! org-mime :pin "b189976217")
(when (featurep! :completion ivy)
  (package! counsel-notmuch :pin "a4a1562935"))
(when (featurep! :completion helm)
  (package! helm-notmuch :pin "97a01497e0"))
