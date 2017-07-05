;; -*- no-byte-compile: t; -*-
;;; org/org-babel/packages.el

(package! ob-go)
(package! ob-mongo)
(package! ob-redis)
(package! ob-restclient)
(package! ob-rust :recipe (:fetcher github :repo "zweifisch/ob-rust"))
(package! ob-sql-mode)
(package! ob-translate)
