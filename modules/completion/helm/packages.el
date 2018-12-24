;; -*- no-byte-compile: t; -*-
;;; completion/helm/packages.el

(package! helm)
(package! helm-ag)
(package! helm-c-yasnippet)
(package! helm-company)
(package! helm-describe-modes :recipe (:fetcher github :repo "emacs-helm/helm-describe-modes"))
(package! helm-projectile)
(package! swiper-helm)
(when (featurep! +fuzzy)
  (package! helm-flx))
(when (and EMACS26+ (featurep! +childframe))
  (package! posframe))
