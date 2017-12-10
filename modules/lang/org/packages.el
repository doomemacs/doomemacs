;; -*- no-byte-compile: t; -*-
;;; lang/org/packages.el

;; NOTE This is an insecure source, but unavoidable if we want org 9.0+ (which
;; this module requires). orgmode.org offers no secure access to this repo. If
;; this bothers you, comment out this `package!' block and download
;; org-plus-contrib from orgmode.org.
(package! org-plus-contrib :recipe (:fetcher git :url "http://orgmode.org/org-mode.git"))

(package! org-bullets :recipe (:fetcher github :repo "hlissner/org-bullets"))
(package! toc-org)

(when (featurep! +attach)
  (package! org-download))

(when (featurep! +babel)
  (package! ob-go)
  (package! ob-mongo)
  (package! ob-redis)
  (package! ob-restclient)
  (package! ob-rust)
  (package! ob-sql-mode)
  (package! ob-translate))

(when (featurep! +export)
  (package! ox-pandoc))

(when (featurep! +present)
  (package! centered-window-mode)
  (package! org-tree-slide)
  (package! ox-reveal))

;; (when (featurep! +publish))
