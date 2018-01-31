;; -*- no-byte-compile: t; -*-
;;; lang/org/packages.el

;; Installs a cutting-edge version of org-mode
(package! org-plus-contrib)

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
  (package! ox-pandoc)
  (package! htmlize))

(when (featurep! +present)
  (package! centered-window :recipe (:fetcher github :repo "anler/centered-window-mode"))
  (package! org-tree-slide)
  (package! ox-reveal))

;; (when (featurep! +publish))
