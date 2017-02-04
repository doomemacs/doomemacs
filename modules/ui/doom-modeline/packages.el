;; -*- no-byte-compile: t; -*-
;;; ui/doom-modeline/packages.el

;;; These are the invisible dependencies
;; Required
;;(require 'f)
;;(require 's)
;;(require 'evil)
;;(require 'projectile)
;;(require 'all-the-icons)

;; Optional
;;(require 'flycheck)
;;(require 'anzu)
;;(require 'evil-anzu)
;;(require 'iedit)
;;(require 'evil-multiedit)

(package! all-the-icons)
(package! eldoc-eval)
(package! evil-anzu :when (featurep 'evil))
(package! powerline)

