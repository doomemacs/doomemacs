;; -*- no-byte-compile: t; -*-
;;; tools/lookup/packages.el

;; HACK `dumb-jump' uses the `helm-build-sync-source' macro, but this requires
;;      helm be loaded before `dumb-jump' is byte-compiled during installation.
;;      To ensure this, we declare helm before dumb-jump.
(when (featurep! :completion helm)
  (package! helm :pin "05d70ff116a696f5c18e5ad569573d8936ff01da"))

;;
(package! dumb-jump :pin "738d40ceb7e2d6a3a26f7f27a31ba93924baaa2d")
(when (featurep! :completion ivy)
  (package! ivy-xref :pin "3d4c35fe2b243d948d8fe02a1f0d76a249d63de9"))
(when (featurep! :completion helm)
  (package! helm-xref :pin "6b4a8bd91f5eaf82f51bd31b03f6587387fe6983"))

(when (featurep! +docsets)
  (package! dash-docs :pin "111fd9b97001f1ad887b45e5308a14ddd68ce70a")
  (when (featurep! :completion helm)
    (package! helm-dash :pin "7f853bd34da666f0e9a883011c80f451b06f6c59"))
  (when (featurep! :completion ivy)
    (package! counsel-dash :pin "370d5f6f14b5294d0eb717f7b2a6a8e93df1ed24")))

(when (featurep! +dictionary)
  (if IS-MAC
      (package! osx-dictionary :pin "1b79ff64c72485cb078db9ab7ee3256b11a99f4b")
    (package! define-word :pin "d8c76d503be3d561221631067ec5274e7c147248"))
  ;; Need for Google/DuckDuckGo auto-completion on `+lookup/online'
  (package! powerthesaurus :pin "81a262ec0c9294ad377bafc6cc4e6d91b461acb6")
  (package! request :pin "b207ebb298dbf181583ebf56c3e18a52fcbaa165"))
