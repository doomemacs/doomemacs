;; -*- no-byte-compile: t; -*-
;;; org/org/packages.el

;; NOTE This is an insecure source, but unavoidable if we want org 9.0+.
;; orgmode.org offers no secure access to this repo. If this bothers you,
;; comment out this `package!' block and download org-plus-contrib from
;; orgmode.org.
(package! org-plus-contrib :recipe (:fetcher git :url "http://orgmode.org/org-mode.git"))

(package! org-bullets :recipe (:fetcher github :repo "hlissner/org-bullets"))
(package! toc-org)
