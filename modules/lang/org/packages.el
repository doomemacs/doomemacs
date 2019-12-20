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
(setq straight-fix-org nil)
(add-hook! 'straight-use-package-pre-build-functions
  (defun +org-fix-package-h (package &rest _)
    (when (member package '("org" "org-plus-contrib"))
      (with-temp-file (expand-file-name "org-version.el" (straight--repos-dir "org"))
        (insert "(fset 'org-release (lambda () \"9.3\"))\n"
                "(fset 'org-git-version #'ignore)\n"
                "(provide 'org-version)\n")))))

(package! org-plus-contrib) ; install cutting-edge version of org-mode

(package! avy)
(package! htmlize)
(package! org-bullets :recipe (:host github :repo "Kaligule/org-bullets"))
(package! org-fancy-priorities)
(package! org-yt :recipe (:host github :repo "TobiasZawada/org-yt"))
(package! ox-clip)
(package! toc-org)
(package! org-cliplink)

(when (featurep! :editor evil +everywhere)
  (package! evil-org :recipe (:host github :repo "hlissner/evil-org-mode")))
(when (featurep! :tools pdf)
  (package! org-pdfview))
(when (featurep! :tools magit)
  (package! orgit))
(when (featurep! +brain)
  (package! org-brain))
(when (featurep! +dragndrop)
  (package! org-download))
(when (featurep! +gnuplot)
  (package! gnuplot)
  (package! gnuplot-mode))
(when (featurep! +ipython) ; DEPRECATED
  (package! ob-ipython))
(when (featurep! +jupyter)
  (package! jupyter))
(when (featurep! +pomodoro)
  (package! org-pomodoro))
(when (featurep! +present)
  (package! centered-window
    :recipe (:host github :repo "anler/centered-window-mode"))
  (package! org-tree-slide)
  (package! org-re-reveal))
(when (featurep! +journal)
  (package! org-journal))

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
(when (featurep! :lang scala)
  (package! ob-ammonite))

;;; Export
(when (featurep! +pandoc)
  (package! ox-pandoc))
(when (featurep! +hugo)
  (package! ox-hugo
    :recipe (:host github :repo "kaushalmodi/ox-hugo" :nonrecursive t)))
(when (featurep! :lang rst)
  (package! ox-rst))
