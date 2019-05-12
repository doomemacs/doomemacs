;; -*- no-byte-compile: t; -*-
;;; tools/lookup/packages.el

;; `dumb-jump' uses the `helm-build-sync-source' macro, but this requires helm
;; be loaded before `dumb-jump' is byte-compiled during installation. To ensure
;; this, we declare helm before dumb-jump.
(when (featurep! :completion helm)
  (package! helm))

;;
(package! dumb-jump)
(when (featurep! :completion ivy)
  (package! ivy-xref))
(when (featurep! :completion helm)
  (package! helm-xref))

(when (featurep! +docsets)
  (package! dash-docs)
  (when (featurep! :completion helm)
    (package! helm-dash))
  (when (featurep! :completion ivy)
    (package! counsel-dash)))
