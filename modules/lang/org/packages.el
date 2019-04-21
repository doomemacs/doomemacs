;; -*- no-byte-compile: t; -*-
;;; lang/org/packages.el

;; Installs a cutting-edge version of org-mode
(package! org-plus-contrib)

;; Prevent built-in Org from playing into the byte-compilation of
;; `org-plus-contrib'.
(when-let* ((orglib (locate-library "org" nil doom-site-load-path)))
  (setq load-path (delete (substring (file-name-directory orglib) 0 -1)
                          load-path)))
;; Ignore org on ELPA, if possible
(package! org :ignore t)

(package! org-bullets :recipe (:fetcher github :repo "Kaligule/org-bullets"))
(package! org-yt :recipe (:fetcher github :repo "TobiasZawada/org-yt"))
(package! toc-org)

(when (featurep! :editor evil)
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
  (package! ox-clip)
  (package! ox-pandoc)
  (package! htmlize))

(when (featurep! +present)
  (package! centered-window :recipe (:fetcher github :repo "anler/centered-window-mode"))
  (package! org-tree-slide)
  (package! ox-reveal))

;; (when (featurep! +publish))
