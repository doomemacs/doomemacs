;; -*- no-byte-compile: t; -*-
;;; lang/racket/packages.el

(package! racket-mode)

(when (featurep! :lang org +babel)
  (package! ob-racket :recipe (:fetcher github :repo "DEADB17/ob-racket")))
