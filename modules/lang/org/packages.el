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
  :pin "9bc0cc7fb3285d9a1eb19bd4ce528e751240311e")
;; ...And prevent other packages from pulling org; org-plus-contrib satisfies
;; the dependency already: https://github.com/raxod502/straight.el/issues/352
(package! org :recipe (:local-repo nil))

(package! avy)
(package! htmlize :pin "86f22f211e9230857197c42a9823d3f05381deed")
(package! org-superstar :pin "09ddc28383d363a4b353348a433e24535b4af0e3")
(package! org-yt
  :recipe (:host github :repo "TobiasZawada/org-yt")
  :pin "40cc1ac76d741055cbefa13860d9f070a7ade001")
(package! ox-clip :pin "bd36f9fb4e3b1b9e8686b993b02ccd780ff75a96")
(package! toc-org :pin "5deaec41ed0e5c51715737d7f74c5ae1b3c00387")
(package! org-cliplink :pin "82402cae7e118d67de7328417fd018a18f95fac2")

(when (featurep! :editor evil +everywhere)
  (package! evil-org
    :recipe (:host github :repo "hlissner/evil-org-mode")
    :pin "9cf661af8ff8ea768ef1e55045be14d0468a90f5"))
(when (featurep! :tools pdf)
  (package! org-pdftools :pin "8cc15bb8014ed1f047eecc0abd8bf447f86c0505"))
(when (featurep! :tools magit)
  (package! orgit :pin "e147f055772cc934fe1f1d8619059badeb647c93"))
(when (featurep! +brain)
  (package! org-brain :pin "3ce2a33b81ce611695ad74bf21cb911ef90d9a1a"))
(when (featurep! +dragndrop)
  (package! org-download :pin "d248fcb8f2592a40507682e91eed9a31ead4e4a6"))
(when (featurep! +gnuplot)
  (package! gnuplot :pin "f0001c30010b2899e36d7d89046322467e923088")
  (package! gnuplot-mode :pin "601f6392986f0cba332c87678d31ae0d0a496ce7"))
(when (featurep! +ipython) ; DEPRECATED
  (package! ob-ipython :pin "7147455230841744fb5b95dcbe03320313a77124"))
(when (featurep! +jupyter)
  (package! jupyter :pin "785edbbff65abb0c929dc2fbd8b8305c77fd529e"))
(when (featurep! +journal)
  (package! org-journal :pin "524009b5daa9ab4740d1c8d91857eb89a6aff07c"))
(when (featurep! +noter)
  (package! org-noter :pin "9ead81d42dd4dd5074782d239b2efddf9b8b7b3d"))
(when (featurep! +pomodoro)
  (package! org-pomodoro :pin "aa07c11318f91219336197e62c47bc7a3d090479"))
(when (featurep! +present)
  (package! centered-window
    :recipe (:host github :repo "anler/centered-window-mode")
    :pin "f50859941ab5c7cbeaee410f2d38716252b552ac")
  (package! org-tree-slide :pin "7bf09a02bd2d8f1ccfcb5209bfb18fbe02d1f44e")
  (package! org-re-reveal :pin "a9e9d4ef88417b3af7741a8d8f444ece820e7a3b"))
(when (featurep! +roam)
  (package! org-roam :pin "e3d101f4958eee16ded089ae27017e3b14c30ccb")
  (when (featurep! :completion company)
    (package! company-org-roam :pin "674c2bd493f571c5323d69279557a6c18ccbd14e")))

;;; Babel
(package! ob-async :pin "80a30b96a007d419ece12c976a81804ede340311")
(when (featurep! :lang crystal)
  (package! ob-crystal :pin "d84c1adee4b269cdba06a97caedb8071561a09af"))
(when (featurep! :lang go)
  (package! ob-go :pin "2067ed55f4c1d33a43cb3f6948609d240a8915f5"))
(when (featurep! :lang hy)
  (package! ob-hy :pin "a42ecaf440adc03e279afe43ee5ef6093ddd542a"))
(when (featurep! :lang nim)
  (package! ob-nim :pin "bf1642cb93f0a898804dc13fd9408d2964403bd2"))
(when (featurep! :lang racket)
  (package! ob-racket
    :recipe (:host github :repo "DEADB17/ob-racket")
    :pin "d8fd51bddb019b0eb68755255f88fc800cfe03cb"))
(when (featurep! :lang rest)
  (package! ob-restclient :pin "f7449b2068498fe9d8ab9589e0a638148861533f"))
(when (featurep! :lang scala)
  (package! ob-ammonite :pin "39937dff395e70aff76a4224fa49cf2ec6c57cca"))

;;; Export
(when (featurep! +pandoc)
  (package! ox-pandoc :pin "aa37dc7e94213d4ebedb85c384c1ba35007da18e"))
(when (featurep! +hugo)
  (package! ox-hugo
    :recipe (:host github :repo "kaushalmodi/ox-hugo" :nonrecursive t)
    :pin "01bc315d49f246d7ecc0bd37ea8ccb870b420a5b"))
(when (featurep! :lang rst)
  (package! ox-rst :pin "9158bfd18096c559e0a225ae62ab683f1c98a547"))
