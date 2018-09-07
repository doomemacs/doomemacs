;; -*- no-byte-compile: t; -*-
;;; feature/lookup/packages.el

;; `dumb-jump' uses the `helm-build-sync-source' macro, but this requires helm
;; be loaded before it is byte-compiled during installation. To ensure this, we
;; declare helm before dumb-jump.
(when (featurep! :completion helm)
  (depends-on! :completion helm))

;;
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
