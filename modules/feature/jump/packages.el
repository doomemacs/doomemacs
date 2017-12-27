;; -*- no-byte-compile: t; -*-
;;; feature/jump/packages.el

(package! dumb-jump)
(package! gxref)
;; (package! ggtags)
;; (cond ((featurep! :completion ivy)
;;        (package! counsel-gtags))
;;       ((featurep! :completion helm)
;;        (package! helm-gtags)))

(when (featurep! :completion ivy)
  (package! ivy-xref))
(when (featurep! :completion helm)
  (package! helm-xref))
