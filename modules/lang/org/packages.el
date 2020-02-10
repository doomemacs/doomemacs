;; -*- no-byte-compile: t; -*-
;;; lang/org/packages.el

;; Prevent built-in Org from playing into the byte-compilation of
;; `org-plus-contrib'.
(when-let (orglib (locate-library "org" nil doom--initial-load-path))
  (setq load-path (delete (substring (file-name-directory orglib) 0 -1)
                          load-path)))

;; HACK A necessary hack because org requires a compilation step after being
;;      cloned, and during that compilation a org-version.el is generated with
;;      these two functions, which return the output of a 'git describe ...'
;;      call in the repo's root. Of course, this command won't work in a sparse
;;      clone, and more than that, initiating these compilation step is a
;;      hassle, so...
(add-hook! 'straight-use-package-pre-build-functions
  (defun +org-fix-package-h (package &rest _)
    (when (equal package "org-mode")
      (with-temp-file (expand-file-name "org-version.el" (straight--repos-dir "org-mode"))
        (insert "(fset 'org-release (lambda () \"9.4\"))\n"
                "(fset 'org-git-version #'ignore)\n"
                "(provide 'org-version)\n")))))

;; Install cutting-edge version of org-mode, and from a mirror, because
;; code.orgmode.org runs on a potato.
(package! org-mode
  :recipe (:host github
           :repo "emacs-straight/org-mode"
           :files ("*.el" "lisp/*.el" "contrib/lisp/*.el"))
  :pin "2096c9c76f")
;; ...And prevent other packages from pulling org; org-plus-contrib satisfies
;; the dependency already: https://github.com/raxod502/straight.el/issues/352
(package! org :recipe (:local-repo nil))

(package! avy)
(package! htmlize :pin "86f22f211e")
(package! org-bullets
  :recipe (:host github :repo "Kaligule/org-bullets")
  :pin "8b4f0aab6d")
(package! org-yt
  :recipe (:host github :repo "TobiasZawada/org-yt")
  :pin "40cc1ac76d")
(package! ox-clip :pin "bd36f9fb4e")
(package! toc-org :pin "379b457fcf")
(package! org-cliplink :pin "82402cae7e")
(package! org-bookmark-heading :pin "38a2813f72")

(when (featurep! :editor evil +everywhere)
  (package! evil-org
    :recipe (:host github :repo "hlissner/evil-org-mode")
    :pin "4d44e9bbdc"))
(when (featurep! :tools pdf)
  (package! org-pdfview :pin "8b71f31363"))
(when (featurep! :tools magit)
  (package! orgit :pin "e7cddf39e3"))
(when (featurep! +brain)
  (package! org-brain :pin "8cb2efc860"))
(when (featurep! +dragndrop)
  (package! org-download :pin "70401884e9"))
(when (featurep! +gnuplot)
  (package! gnuplot :pin "a406143d52")
  (package! gnuplot-mode :pin "601f639298"))
(when (featurep! +ipython) ; DEPRECATED
  (package! ob-ipython :pin "7147455230"))
(when (featurep! +jupyter)
  (package! jupyter :pin "9e3c163358"))
(when (featurep! +pomodoro)
  (package! org-pomodoro :pin "aa07c11318"))
(when (featurep! +present)
  (package! centered-window
    :recipe (:host github :repo "anler/centered-window-mode")
    :pin "24f7c5be9d")
  (package! org-tree-slide :pin "7bf09a02bd")
  (package! org-re-reveal :pin "14df7542f2"))
(when (featurep! +journal)
  (package! org-journal :pin "128f0533a7"))

;;; Babel
(package! ob-async :pin "80a30b96a0")
(when (featurep! :lang crystal)
  (package! ob-crystal :pin "d84c1adee4"))
(when (featurep! :lang go)
  (package! ob-go :pin "2067ed55f4"))
(when (featurep! :lang nim)
  (package! ob-nim :pin "bf1642cb93"))
(when (featurep! :lang racket)
  (package! ob-racket
    :recipe (:host github :repo "DEADB17/ob-racket")
    :pin "d8fd51bddb"))
(when (featurep! :lang rest)
  (package! ob-restclient :pin "c5c22e6035"))
(when (featurep! :lang rust)
  (package! ob-rust :pin "6a82587598"))
(when (featurep! :lang scala)
  (package! ob-ammonite :pin "39937dff39"))

;;; Export
(when (featurep! +pandoc)
  (package! ox-pandoc :pin "aa37dc7e94"))
(when (featurep! +hugo)
  (package! ox-hugo
    :recipe (:host github :repo "kaushalmodi/ox-hugo" :nonrecursive t)
    :pin "a80b250987"))
(when (featurep! :lang rst)
  (package! ox-rst :pin "9158bfd180"))
