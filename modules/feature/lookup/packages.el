;; -*- no-byte-compile: t; -*-
;;; feature/lookup/packages.el

(package! dumb-jump)
(when (featurep! :completion ivy)
  (package! ivy-xref))
(when (featurep! :completion helm)
  (package! helm-xref))

(when (featurep! +docsets)
  (when (featurep! :completion helm)
    (package! helm-dash))
  (when (featurep! :completion ivy)
    (package! counsel-dash)))

(when (featurep! +devdocs)
  (package! devdocs))
