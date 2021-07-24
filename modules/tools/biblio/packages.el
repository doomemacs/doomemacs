;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "a0d32ab16748b7b0c43d6421f1b497b7caf8e590")
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "a0d32ab16748b7b0c43d6421f1b497b7caf8e590"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "a0d32ab16748b7b0c43d6421f1b497b7caf8e590"))
(when (featurep! :completion vertico)
  (package! bibtex-actions :pin "6e3a194c3ab655693f8194be78542366755c58c9"))
