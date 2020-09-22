;; -*- no-byte-compile: t; -*-
;;; lang/org/packages.el

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
  :pin "220f2b0d93a6927eb673978c0042a1d4673e86aa"
  ;; Prevents built-in Org from sneaking into the byte-compilation of
  ;; `org-plus-contrib', and inform other packages that `org-mode' satisfies the
  ;; `org' dependency: https://github.com/raxod502/straight.el/issues/352
  :shadow 'org)

(package! avy)
(package! htmlize :pin "49205105898ba8993b5253beec55d8bddd820a70")
(package! org-yt
  :recipe (:host github :repo "TobiasZawada/org-yt")
  :pin "40cc1ac76d741055cbefa13860d9f070a7ade001")
(package! ox-clip :pin "f5eac28734ea33d0b7a3dbe10b777907a91cf9f9")
(package! toc-org :pin "5deaec41ed0e5c51715737d7f74c5ae1b3c00387")
(package! org-cliplink :pin "82402cae7e118d67de7328417fd018a18f95fac2")

(when (featurep! :editor evil +everywhere)
  (package! evil-org
    :recipe (:host github :repo "hlissner/evil-org-mode")
    :pin "2e9c4a295ee6aea7c97c5b1f3892b1c6e28a32d9"))
(when (featurep! :tools pdf)
  (package! org-pdftools :pin "8cc15bb8014ed1f047eecc0abd8bf447f86c0505"))
(when (featurep! :tools magit)
  (package! orgit :pin "ac9b1a42863a864fde9d225890ef5464bffdc646"))
(when (featurep! +brain)
  (package! org-brain :pin "671db0e08b91c7d2637d765a7afca8b2561275c8"))
(when (featurep! +dragndrop)
  (package! org-download :pin "67b3c744f94cf0bf50f7052ce428e95af5a6ff3f"))
(when (featurep! +gnuplot)
  (package! gnuplot :pin "f0001c30010b2899e36d7d89046322467e923088")
  (package! gnuplot-mode :pin "601f6392986f0cba332c87678d31ae0d0a496ce7"))
(when (featurep! +ipython) ; DEPRECATED
  (package! ob-ipython :pin "7147455230841744fb5b95dcbe03320313a77124"))
(when (featurep! +jupyter)
  (package! jupyter :pin "360cae2c70ab28c7a7848c0c56473d984f0243e5"))
(when (featurep! +journal)
  (package! org-journal :pin "a2728e25b854af21cea8e8d313714c993eb1c848"))
(when (featurep! +noter)
  (package! org-noter :pin "9ead81d42dd4dd5074782d239b2efddf9b8b7b3d"))
(when (featurep! +pomodoro)
  (package! org-pomodoro :pin "aa07c11318f91219336197e62c47bc7a3d090479"))
(when (featurep! +pretty)
  (package! org-superstar :pin "94f35c20f8b84a63defa145e3e6ae735fa33dd5d")
  (package! org-fancy-priorities :pin "819bb993b71e7253cefef7047306ab4e0f9d0a86"))
(when (featurep! +present)
  (package! centered-window
    :recipe (:host github :repo "anler/centered-window-mode")
    :pin "f50859941ab5c7cbeaee410f2d38716252b552ac")
  (package! org-tree-slide :pin "7126a4365072a32898f169ead8fb59265dabc605")
  (package! org-re-reveal :pin "7fe39d5d03ccc75d2811445d25cbbb473b53de76")
  (package! revealjs
    :recipe (:host github :repo "hakimel/reveal.js"
             :files ("css" "dist" "js" "plugin"))
    :pin "15815efe05ca69c35ce66cfdbf93316e1db66ecb"))
(when (featurep! +roam)
  (package! org-roam :pin "c33867e6bc282ff0a69d4ef4a020db82604039bb")
  (when (featurep! :completion company)
    (package! company-org-roam :pin "1132663bd68022aa7ea005ff53c7c7571890769d")))

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
    :pin "75b849e9561c4a6022babf6eaf0e037310ded7c1"))
(when (featurep! :lang rst)
  (package! ox-rst :pin "99fa790da55b57a3f2e9aa187493ba434a64250e"))
