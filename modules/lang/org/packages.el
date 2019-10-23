;; -*- no-byte-compile: t; -*-
;;; lang/org/packages.el

;; Prevent built-in Org from playing into the byte-compilation of
;; `org-plus-contrib'.
(when-let (orglib (locate-library "org" nil doom--initial-load-path))
  (setq load-path (delete (substring (file-name-directory orglib) 0 -1)
                          load-path)))

(package! org-plus-contrib)  ; install cutting-edge version of org-mode
(package! org-bullets :recipe (:host github :repo "Kaligule/org-bullets"))
(package! toc-org)
(when (featurep! :editor evil +everywhere)
  (package! evil-org :recipe (:host github :repo "hlissner/evil-org-mode")))
(when (featurep! :tools pdf)
  (package! org-pdfview))
(package! htmlize)
(package! ox-clip)
(package! org-yt :recipe (:host github :repo "TobiasZawada/org-yt"))
(when (featurep! :tools magit)
  (package! orgit))

;;; Babel
(package! ob-async)
(when (featurep! :lang crystal)
  (package! ob-crystal))
(when (featurep! :lang go)
  (package! ob-go))
(when (featurep! :lang nim)
  (package! ob-nim))
(when (featurep! :lang racket)
  (package! ob-racket :recipe (:host github :repo "DEADB17/ob-racket")))
(when (featurep! :lang rest)
  (package! ob-restclient))
(when (featurep! :lang rust)
  (package! ob-rust))

;;; Modules
(when (featurep! +dragndrop)
  (package! org-download))
(when (featurep! +gnuplot)
  (package! gnuplot)
  (package! gnuplot-mode))
(when (featurep! +ipython)
  (package! ob-ipython))
(when (featurep! +pandoc)
  (package! ox-pandoc))
(when (featurep! +pomodoro)
  (package! org-pomodoro))
(when (featurep! +present)
  (package! centered-window :recipe (:host github :repo "anler/centered-window-mode"))
  (package! org-tree-slide)
  (package! org-re-reveal))

(when (featurep! +journal)
  (package! org-journal))

(when (featurep! +hugo)
  (package! ox-hugo :recipe (:host github
                             :repo "kaushalmodi/ox-hugo"
                             :nonrecursive t)))
