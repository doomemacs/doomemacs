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
  :pin "b68090e0be889b7e797614b97db92bb2c70eadbe")
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
    :pin "2e9c4a295ee6aea7c97c5b1f3892b1c6e28a32d9"))
(when (featurep! :tools pdf)
  (package! org-pdftools :pin "8cc15bb8014ed1f047eecc0abd8bf447f86c0505"))
(when (featurep! :tools magit)
  (package! orgit :pin "e147f055772cc934fe1f1d8619059badeb647c93"))
(when (featurep! +brain)
  (package! org-brain :pin "2c86ec8b9dc51bf15ab1a4e0eb7024ba627a1c99"))
(when (featurep! +dragndrop)
  (package! org-download :pin "40c8a1db186a4ec79d87805018237234c0aad878"))
(when (featurep! +gnuplot)
  (package! gnuplot :pin "f0001c30010b2899e36d7d89046322467e923088")
  (package! gnuplot-mode :pin "601f6392986f0cba332c87678d31ae0d0a496ce7"))
(when (featurep! +ipython) ; DEPRECATED
  (package! ob-ipython :pin "7147455230841744fb5b95dcbe03320313a77124"))
(when (featurep! +jupyter)
  (package! jupyter :pin "785edbbff65abb0c929dc2fbd8b8305c77fd529e"))
(when (featurep! +journal)
  (package! org-journal :pin "1d8e8e6836c1dde437126bd76f622838f75858f3"))
(when (featurep! +noter)
  (package! org-noter :pin "9ead81d42dd4dd5074782d239b2efddf9b8b7b3d"))
(when (featurep! +pomodoro)
  (package! org-pomodoro :pin "aa07c11318f91219336197e62c47bc7a3d090479"))
(when (featurep! +present)
  (package! centered-window
    :recipe (:host github :repo "anler/centered-window-mode")
    :pin "f50859941ab5c7cbeaee410f2d38716252b552ac")
  (package! org-tree-slide :pin "7151aaf4df27b9b7339e277cfa1f8341c3f11231")
  (package! org-re-reveal :pin "a9e9d4ef88417b3af7741a8d8f444ece820e7a3b"))
(when (featurep! +roam)
  (package! org-roam :pin "c46d153fd382c4ee5a91b694d1043a3a8c990597")
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
    :pin "f8e26aaee92491ca348c2b6f7fb49627642b3176"))
(when (featurep! :lang rst)
  (package! ox-rst :pin "9158bfd18096c559e0a225ae62ab683f1c98a547"))
