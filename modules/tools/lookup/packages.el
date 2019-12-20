;; -*- no-byte-compile: t; -*-
;;; tools/lookup/packages.el

;; HACK `dumb-jump' uses the `helm-build-sync-source' macro, but this requires
;;      helm be loaded before `dumb-jump' is byte-compiled during installation.
;;      To ensure this, we declare helm before dumb-jump.
(when (featurep! :completion helm)
  (package! helm))

;;
(package! dumb-jump)
(when (featurep! :completion ivy)
  (package! ivy-xref)
  ;; Need for Google/DuckDuckGo auto-completion on `+lookup/online'
  (package! request))
(when (featurep! :completion helm)
  (package! helm-google)
  (package! helm-xref))

(when (featurep! +docsets)
  (package! dash-docs)
  (when (featurep! :completion helm)
    (package! helm-dash))
  (when (featurep! :completion ivy)
    (package! counsel-dash)))

(when (featurep! +dictionary)
  (if IS-MAC
      (package! osx-dictionary)
    (package! define-word))
  (package! powerthesaurus))
