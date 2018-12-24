;; -*- no-byte-compile: t; -*-
;;; lang/org/packages.el

;; Installs a cutting-edge version of org-mode
(package! org-plus-contrib)
(package! org :ignore t) ; ignore org from ELPA

(package! org-bullets :recipe (:fetcher github :repo "Kaligule/org-bullets"))
(package! org-yt :recipe (:fetcher github :repo "TobiasZawada/org-yt"))
(package! toc-org)

(when (featurep! :feature evil)
  (package! evil-org))

(when (featurep! :tools pdf)
  (package! org-pdfview))

(when (featurep! +attach)
  (package! org-download))

(when (featurep! +babel)
  (package! ob-async)
  (package! ob-mongo)
  (package! ob-sql-mode)
  (package! ob-translate)

  (when (featurep! +ipython)
    (package! ob-ipython))

  (when (featurep! :lang crystal)
    (package! ob-crystal))
  (when (featurep! :lang go)
    (package! ob-go))
  (when (featurep! :lang nim)
    (package! ob-nim))
  (when (featurep! :lang racket)
    (package! ob-racket :recipe (:fetcher github :repo "DEADB17/ob-racket")))
  (when (featurep! :lang rest)
    (package! ob-restclient))
  (when (featurep! :lang rust)
    (package! ob-rust)))

(when (featurep! +export)
  (package! ox-pandoc)
  (package! htmlize))

(when (featurep! +present)
  (package! centered-window :recipe (:fetcher github :repo "anler/centered-window-mode"))
  (package! org-tree-slide)
  (package! ox-reveal))

;; (when (featurep! +publish))
