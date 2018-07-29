;; -*- no-byte-compile: t; -*-
;;; lang/racket/packages.el

(package! racket-mode)
(package! ob-racket
  :recipe (:fetcher github
                    :repo "DEADB17/ob-racket"
                    :files ("*")))
