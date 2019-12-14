;; -*- no-byte-compile: t; -*-
;;; ui/modeline/packages.el

(unless (featurep! +light)
  (package! doom-modeline))
(package! anzu)
(when (featurep! :editor evil)
  (package! evil-anzu))
