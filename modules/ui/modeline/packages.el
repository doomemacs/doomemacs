;; -*- no-byte-compile: t; -*-
;;; ui/modeline/packages.el

;;; These are the invisible dependencies
;; Required
;;(require 'evil)
;;(require 'projectile)
;;(require 'all-the-icons)

;; Optional
;;(require 'flycheck)
;;(require 'iedit)
;;(require 'evil-multiedit)

(package! anzu)

(when (featurep! :feature evil)
  (package! evil-anzu))

(package! shrink-path)
