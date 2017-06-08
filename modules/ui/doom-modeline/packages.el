;; -*- no-byte-compile: t; -*-
;;; ui/doom-modeline/packages.el

;;; These are the invisible dependencies
;; Required
;;(require 'evil)
;;(require 'projectile)
;;(require 'all-the-icons)

;; Optional
;;(require 'flycheck)
;;(require 'anzu)
;;(require 'iedit)
;;(require 'evil-multiedit)

(package! eldoc-eval)
(when (featurep! :feature evil)
  (package! evil-anzu))

