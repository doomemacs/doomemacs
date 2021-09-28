;; -*- no-byte-compile: t; -*-
;;; lang/beancount/packages.el

(package! beancount
  :recipe (:host github
           :repo "beancount/beancount-mode")
  :pin "dbafe6a73d90c1f64d457b356b9dbb43499f70d5")
