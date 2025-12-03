;; -*- no-byte-compile: t; -*-
;;; lang/beancount/packages.el

(package! beancount
  :recipe (:host github
           :repo "beancount/beancount-mode")
  :pin "8a564f5a26e6245860188ebf71db0262dd78e068")
