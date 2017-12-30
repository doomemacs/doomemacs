;; -*- no-byte-compile: t; -*-
;;; lang/org/packages.el

(when (version< emacs-version "26.1")
  ;; We want org 9.1.x, but the org packaged with Emacs 25.x and under is 8.x.
  ;; The only secure (and reasonably trustworthy) source for this is via
  ;; emacsmirror. Emacs 26+ comes with Org 9.1.4.
  (package! org-plus-contrib
    :recipe (:fetcher github :repo "emacsmirror/org" :files (:defaults "contrib/lisp/*.el"))))

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
