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

(package! powerline :demand t)

(package! all-the-icons :when (display-graphic-p) :demand t)

(package! eldoc-eval :demand t)

