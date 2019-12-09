;; -*- no-byte-compile: t; -*-
;;; completion/helm/packages.el

(package! helm)
(package! helm-rg)
(package! helm-c-yasnippet)
(package! helm-company)
(package! helm-describe-modes :recipe (:host github :repo "emacs-helm/helm-describe-modes"))
(package! helm-projectile)
(package! swiper-helm)
(when (featurep! +fuzzy)
  (package! helm-flx))
(when (featurep! +childframe)
  (package! posframe))
(when (featurep! :lang org)
  (package! helm-org))
