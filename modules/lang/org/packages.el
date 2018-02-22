;; -*- no-byte-compile: t; -*-
;;; lang/org/packages.el

;; Installs a cutting-edge version of org-mode
(package! org-plus-contrib)

(package! org-bullets :recipe (:fetcher github :repo "Kaligule/org-bullets"))
(package! toc-org)

(when (featurep! :feature evil)
  (package! evil-org))

(when (featurep! +attach)
  (package! org-download))

(when (featurep! +babel)
  (package! ob-mongo)
  (package! ob-sql-mode)
  (package! ob-translate)

  (when (featurep! :lang crystal)
    (package! ob-crystal))
  (when (featurep! :lang go)
    (package! ob-go))
  (when (featurep! :lang rust)
    (package! ob-rust))
  (when (featurep! :lang restclient)
    (package! ob-restclient))
  (when (featurep! :lang crystal)
    (package! ob-crystal)))

(when (featurep! +export)
  (package! ox-pandoc)
  (package! htmlize))

(when (featurep! +present)
  (package! centered-window :recipe (:fetcher github :repo "anler/centered-window-mode"))
  (package! org-tree-slide)
  (package! ox-reveal))

;; (when (featurep! +publish))
