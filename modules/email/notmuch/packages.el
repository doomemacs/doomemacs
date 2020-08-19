;; -*- no-byte-compile: t; -*-
;;; email/notmuch/packages.el

(package! notmuch :pin "8776faf6d5118e8152ecfacf94accf35ccebf1d2")
(package! org-mime :pin "9bb6351b25c62835c7881fc64096028eb8ef83ef")
(when (featurep! :completion ivy)
  (package! counsel-notmuch :pin "a4a1562935e4180c42524c51609d1283e9be0688"))
(when (featurep! :completion helm)
  (package! helm-notmuch :pin "97a01497e079a7b6505987e9feba6b603bbec288"))
