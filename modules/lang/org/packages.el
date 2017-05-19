;; -*- no-byte-compile: t; -*-
;;; lang/org/packages.el

;; NOTE This is an insecure source, but unavoidable if we want org 9.0+.
;; orgmode.org offers no secure access to this repo. If this bothers you,
;; comment out this package! block and download org-plus-contrib from
;; orgmode.org.
(package! org-plus-contrib :recipe (:fetcher git :url "http://orgmode.org/org-mode.git"))

(package! org-download)
(package! org-bullets)
(package! ob-go)
(package! ob-mongo)
(package! ob-redis)
(package! ob-restclient)
(package! ob-rust :recipe (:fetcher github :repo "zweifisch/ob-rust"))
(package! ob-sql-mode)
(package! ob-translate)
;; (package! ox-pandox)
