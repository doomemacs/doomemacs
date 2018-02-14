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

(when (featurep! :feature evil)
  (package! evil-anzu))

(package! shrink-path)
