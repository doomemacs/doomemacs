;; -*- no-byte-compile: t; -*-
;;; email/notmuch/packages.el

(package! notmuch :pin "45193bab16c728ba892a5d45fc62ef59e2a6ef85")
(package! org-mime :pin "eb21c02ba8f97fe69c14dc657a7883b982664649")
(when (featurep! :completion ivy)
  (package! counsel-notmuch :pin "a4a1562935e4180c42524c51609d1283e9be0688"))
(when (featurep! :completion helm)
  (package! helm-notmuch :pin "97a01497e079a7b6505987e9feba6b603bbec288"))
