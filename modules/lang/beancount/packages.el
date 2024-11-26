;; -*- no-byte-compile: t; -*-
;;; lang/beancount/packages.el

(package! beancount
  :recipe (:host github
           :repo "beancount/beancount-mode")
  :pin "452621fa1f918d8a105e09d3bd82b2deb45b7146")
