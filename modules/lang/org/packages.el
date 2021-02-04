;; -*- no-byte-compile: t; -*-
;;; lang/org/packages.el

;; Install cutting-edge version of org-mode, and from a mirror, because
;; code.orgmode.org runs on a potato.
(package! org-mode
  :recipe (:host github
           :repo "emacs-straight/org-mode"
           :files ("*.el" "lisp/*.el" "contrib/lisp/*.el")
           ;; HACK A necessary hack because org requires a compilation step
           ;;      after being cloned, and during that compilation a
           ;;      org-version.el is generated with these two functions, which
           ;;      return the output of a 'git describe ...'  call in the repo's
           ;;      root. Of course, this command won't work in a sparse clone,
           ;;      and more than that, initiating these compilation step is a
           ;;      hassle, so...
           :pre-build
           (with-temp-file (doom-path (straight--repos-dir "org-mode") "org-version.el")
             (insert "(fset 'org-release (lambda () \"9.5\"))\n"
                     "(fset 'org-git-version #'ignore)\n"
                     "(provide 'org-version)\n")))
  :pin "6b83c6e4eaec4af47a90d05c3410d4637d8cb8da"
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
(package! toc-org :pin "aef220c266f53d36055f74f4a243c6483c563d2a")
(package! org-cliplink :pin "13e0940b65d22bec34e2de4bc8cba1412a7abfbc")

(when (featurep! :editor evil +everywhere)
  (package! evil-org
    :recipe (:host github :repo "hlissner/evil-org-mode")
    :pin "a9706da260c45b98601bcd72b1d2c0a24a017700"))
(when (featurep! :tools pdf)
  (package! org-pdftools :pin "bcf0084883ede36e91c72be73c0fbd7098439c99"))
(when (featurep! :tools magit)
  (package! orgit :pin "ac9b1a42863a864fde9d225890ef5464bffdc646"))
(when (featurep! +brain)
  (package! org-brain :pin "f7939ef5071895930eebccf490ea7cb25cc54b2c"))
(when (featurep! +dragndrop)
  (package! org-download :pin "97bec7412e1a4d6e9031c7a0568d0f065cd9fd00"))
(when (featurep! +gnuplot)
  (package! gnuplot :pin "116cad8e09024223f97e81b0a4503cef20de9bf5")
  (package! gnuplot-mode :pin "601f6392986f0cba332c87678d31ae0d0a496ce7"))
(when (featurep! +ipython) ; DEPRECATED
  (package! ob-ipython :pin "7147455230841744fb5b95dcbe03320313a77124"))
(when (featurep! +jupyter)
  (package! jupyter :pin "360cae2c70ab28c7a7848c0c56473d984f0243e5"))
(when (featurep! +journal)
  (package! org-journal :pin "c0836483ae43e525bf7547b7a789d171eda84c84"))
(when (featurep! +noter)
  (package! org-noter :pin "9ead81d42dd4dd5074782d239b2efddf9b8b7b3d"))
(when (featurep! +pomodoro)
  (package! org-pomodoro :pin "aa07c11318f91219336197e62c47bc7a3d090479"))
(when (featurep! +pretty)
  (package! org-superstar :pin "7f83636db215bf5a10edbfdf11d12a132864a914")
  (package! org-fancy-priorities :pin "819bb993b71e7253cefef7047306ab4e0f9d0a86"))
(when (featurep! +present)
  (package! centered-window
    :recipe (:host github :repo "anler/centered-window-mode")
    :pin "f50859941ab5c7cbeaee410f2d38716252b552ac")
  (package! org-tree-slide :pin "d6e8e91433dfe4968f1343b483f2680f45a77d52")
  (package! org-re-reveal :pin "47339ef6772c79849a9764716df8361649ea7bdc")
  (package! revealjs
    :recipe (:host github :repo "hakimel/reveal.js"
             :files ("css" "dist" "js" "plugin"))
    :pin "0582f57517c97a4c7bfeb58762138c78883f94c5"))
(when (featurep! +roam)
  (package! org-roam :pin "15d864a500d90c9dc2e16d888e93343528ec3941"))

;;; Babel
(package! ob-async :pin "de1cd6c93242a4cb8773bbe115b7be3d4dd6b97e")
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
  (package! ob-restclient :pin "0ebfc7c5ebf96d2fe1a476439831363a5a43b9b6"))
(when (featurep! :lang scala)
  (package! ob-ammonite :pin "39937dff395e70aff76a4224fa49cf2ec6c57cca"))

;;; Export
(when (featurep! +pandoc)
  (package! ox-pandoc :pin "aa37dc7e94213d4ebedb85c384c1ba35007da18e"))
(when (featurep! +hugo)
  (package! ox-hugo
    :recipe (:host github :repo "kaushalmodi/ox-hugo" :nonrecursive t)
    :pin "6bc8ee08023695fa167ac0ddf1fc61e1975fa1ce"))
(when (featurep! :lang rst)
  (package! ox-rst :pin "99fa790da55b57a3f2e9aa187493ba434a64250e"))
