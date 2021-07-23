;; -*- no-byte-compile: t; -*-
;;; lang/beancount/packages.el

(package! beancount
  :recipe (:host github
           :repo "beancount/beancount-mode")
  :pin "02fe03ee4487de7f83f587db65970b91ebd2d9d7")
