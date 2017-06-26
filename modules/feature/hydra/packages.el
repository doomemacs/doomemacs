;; -*- no-byte-compile: t; -*-
;;; feature/hydra/packages.el

(package! hydra)
(when (featurep! :completion ivy)
  (package! ivy-hydra))

