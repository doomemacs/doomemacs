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
    (when (member package '("org" "org-plus-contrib"))
      (with-temp-file (expand-file-name "org-version.el" (straight--repos-dir "org"))
        (insert "(fset 'org-release (lambda () \"9.3\"))\n"
                "(fset 'org-git-version #'ignore)\n"
                "(provide 'org-version)\n")))))

;; install cutting-edge version of org-mode
(package! org-plus-contrib :pin "0ac6a9e1fcb71415df5ce287d4658f6a601b3df3")
;; ...And prevent other packages from pulling org; org-plus-contrib satisfies
;; the dependency already: https://github.com/raxod502/straight.el/issues/352
(package! org :recipe (:local-repo nil) :pin "0ac6a9e1fcb71415df5ce287d4658f6a601b3df3")

(package! avy :pin "cf95ba9582121a1c2249e3c5efdc51acd566d190")
(package! htmlize :pin "86f22f211e9230857197c42a9823d3f05381deed")
(package! org-bullets
  :recipe (:host github :repo "Kaligule/org-bullets")
  :pin "8b4f0aab6d49b00faa779785b978fdb67e2eb066")
(package! org-fancy-priorities :pin "819bb993b71e7253cefef7047306ab4e0f9d0a86")
(package! org-yt
  :recipe (:host github :repo "TobiasZawada/org-yt")
  :pin "40cc1ac76d741055cbefa13860d9f070a7ade001")
(package! ox-clip :pin "bd36f9fb4e3b1b9e8686b993b02ccd780ff75a96")
(package! toc-org :pin "379b457fcff091d2fa47223ade58f457fd6eed28")
(package! org-cliplink :pin "82402cae7e118d67de7328417fd018a18f95fac2")
(package! org-bookmark-heading :pin "38a2813f72ff65f3ae91e2ebb23e0bbb42a8d1df")

(when (featurep! :editor evil +everywhere)
  (package! evil-org
    :recipe (:host github :repo "hlissner/evil-org-mode")
    :pin "4d44e9bbdc3ae35d0050ca298886710f6531f434"))
(when (featurep! :tools pdf)
  (package! org-pdfview :pin "8b71f313634b95a1fac42fc701934fd796565f3b"))
(when (featurep! :tools magit)
  (package! orgit :pin "e7cddf39e301c87c36c7de13e429dee74874d5c8"))
(when (featurep! +brain)
  (package! org-brain :pin "8cb2efc86026f0dcd19a63aef97044131682eba5"))
(when (featurep! +dragndrop)
  (package! org-download :pin "a367669384859261bcb11bac4b782f231f972353"))
(when (featurep! +gnuplot)
  (package! gnuplot :pin "a406143d52618638d908b6b0b1c1c90c045b83ee")
  (package! gnuplot-mode :pin "601f6392986f0cba332c87678d31ae0d0a496ce7"))
(when (featurep! +ipython) ; DEPRECATED
  (package! ob-ipython :pin "7147455230841744fb5b95dcbe03320313a77124"))
(when (featurep! +jupyter)
  (package! jupyter :pin "9e3c1633586982e278f072dfaaabd115fa4d19f7"))
(when (featurep! +pomodoro)
  (package! org-pomodoro :pin "aa07c11318f91219336197e62c47bc7a3d090479"))
(when (featurep! +present)
  (package! centered-window
    :recipe (:host github :repo "anler/centered-window-mode")
    :pin "24f7c5be9def20879f46659082d497e67b55d7af")
  (package! org-tree-slide :pin "7bf09a02bd2d8f1ccfcb5209bfb18fbe02d1f44e")
  (package! org-re-reveal :pin "29bc467201220dbf5091fe2d32a2b237c744ff10"))
(when (featurep! +journal)
  (package! org-journal :pin "cf0f15386fb52479f3b8f4f494feff71ba0052a4"))

;;; Babel
(package! ob-async :pin "80a30b96a007d419ece12c976a81804ede340311")
(when (featurep! :lang crystal)
  (package! ob-crystal :pin "d84c1adee4b269cdba06a97caedb8071561a09af"))
(when (featurep! :lang go)
  (package! ob-go :pin "2067ed55f4c1d33a43cb3f6948609d240a8915f5"))
(when (featurep! :lang nim)
  (package! ob-nim :pin "bf1642cb93f0a898804dc13fd9408d2964403bd2"))
(when (featurep! :lang racket)
  (package! ob-racket
    :recipe (:host github :repo "DEADB17/ob-racket")
    :pin "d8fd51bddb019b0eb68755255f88fc800cfe03cb"))
(when (featurep! :lang rest)
  (package! ob-restclient :pin "c5c22e603531dca48575d0a425fddff16dc0f391"))
(when (featurep! :lang rust)
  (package! ob-rust :pin "6a82587598cd097e9642be916243c31f1231b24a"))
(when (featurep! :lang scala)
  (package! ob-ammonite :pin "39937dff395e70aff76a4224fa49cf2ec6c57cca"))

;;; Export
(when (featurep! +pandoc)
  (package! ox-pandoc :pin "aa37dc7e94213d4ebedb85c384c1ba35007da18e"))
(when (featurep! +hugo)
  (package! ox-hugo
    :recipe (:host github :repo "kaushalmodi/ox-hugo" :nonrecursive t)
    :pin "d2892b3b5ea19f85063f2fba4a5b7ffa1123a395"))
(when (featurep! :lang rst)
  (package! ox-rst :pin "9158bfd18096c559e0a225ae62ab683f1c98a547"))
