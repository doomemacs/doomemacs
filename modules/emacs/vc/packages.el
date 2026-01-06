;; -*- no-byte-compile: t; -*-
;;; emacs/vc/packages.el

(package! vc :built-in t)
(package! vc-annotate :built-in t)
(package! smerge-mode :built-in t)

(package! browse-at-remote :pin "27b17cc63b9f9dca893425908373251eb9b10f44")
(package! git-timemachine
  ;; The original lives on codeberg.org; which has uptime issues.
  :recipe (:host github :repo "emacsmirror/git-timemachine")
  :pin "d1346a76122595aeeb7ebb292765841c6cfd417b")
(package! git-modes :pin "c3faeeea1982786f78d8c38397dec0f078eaec84") ; v1.4.8
