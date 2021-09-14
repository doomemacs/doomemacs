;; -*- no-byte-compile: t; -*-
;;; lang/org/packages.el

(package! org
  :recipe (:host github
           ;; Install cutting-edge version of org, and from a mirror because
           ;; code.orgmode.org's uptime is worse than Github's, and
           ;; emacs-straight/org is smaller and, therefore, quicker to download.
           :repo "emacs-straight/org"
           :files (:defaults "etc")
           ;; HACK A necessary hack because org requires a compilation step
           ;;      after being cloned, and during that compilation a
           ;;      org-version.el is generated with these two functions, which
           ;;      return the output of a 'git describe ...' call in the repo's
           ;;      root. Of course, this command won't work in a sparse clone,
           ;;      and initiating these compilation step is a hassle, so...
           :build t
           :pre-build
           (with-temp-file "org-version.el"
             (insert "(defun org-release () \"9.5\")\n"
                     (format "(defun org-git-version (&rest _) \"9.5-%s\")\n"
                             (cdr (doom-call-process "git" "rev-parse" "--short" "HEAD")))
                     "(provide 'org-version)\n")))
  :pin "888aaa97c0ce331097787333d0d712dd6e4d078f")
(package! org-contrib
  :recipe (:host nil :repo "https://git.sr.ht/~bzg/org-contrib")
  :pin "fc81309cf6756607a836f93049a9393c2967c4e0")

(package! avy)
(package! htmlize :pin "dd27bc3f26efd728f2b1f01f9e4ac4f61f2ffbf9")
(package! org-yt
  :recipe (:host github :repo "TobiasZawada/org-yt")
  :pin "40cc1ac76d741055cbefa13860d9f070a7ade001")
(package! ox-clip :pin "05a14d56bbffe569d86f20b49ae31ed2ac7d1101")
(package! toc-org :pin "df4ad6ff15e3b02f6322305638a441a636b9b37e")
(package! org-cliplink :pin "13e0940b65d22bec34e2de4bc8cba1412a7abfbc")

(when (featurep! :editor evil +everywhere)
  (package! evil-org
    :recipe (:host github :repo "hlissner/evil-org-mode")
    :pin "a9706da260c45b98601bcd72b1d2c0a24a017700"))
(when (featurep! :tools pdf)
  (package! org-pdftools :pin "a5b61bca3f8c91b0859bb0df1a929f9a31a57b99"))
(when (featurep! :tools magit)
  (package! orgit :pin "f956d802f19ea495efa95af6c673588afeb3adc5")
  (when (featurep! :tools magit +forge)
    (package! orgit-forge :pin "365b75609a9454dccf5681eb6075ca53bd32af85")))
(when (featurep! +brain)
  (package! org-brain :pin "46ca9f766322cff31279ecdf02251ff24a0e9431"))
(when (featurep! +dragndrop)
  (package! org-download :pin "947ca223643d28e189480e607df68449c15786cb"))
(when (featurep! +gnuplot)
  (package! gnuplot :pin "7138b139d2dca9683f1a81325c643b2744aa1ea3")
  (package! gnuplot-mode :pin "601f6392986f0cba332c87678d31ae0d0a496ce7"))
(when (featurep! +ipython) ; DEPRECATED
  (package! ob-ipython :pin "7147455230841744fb5b95dcbe03320313a77124"))
(when (featurep! +jupyter)
  (package! jupyter :pin "1f0612eb936d36abab0f27b09cca691e81fc6e74"))
(when (featurep! +journal)
  (package! org-journal :pin "9757996ca058029800c4801fba315b1d1614dcb2"))
(when (featurep! +noter)
  (package! org-noter :pin "9ead81d42dd4dd5074782d239b2efddf9b8b7b3d"))
(when (featurep! +pomodoro)
  (package! org-pomodoro :pin "aa07c11318f91219336197e62c47bc7a3d090479"))
(when (featurep! +pretty)
  (package! org-appear :pin "a1aa8496f2fd61305e43e03e6eeee2ff92aa9e24")
  (package! org-superstar :pin "2cd3f1e74b88a846901da3249edb3a71d8e58c55")
  (package! org-fancy-priorities :pin "7f677c6c14ecf05eab8e0efbfe7f1b00ae68eb1d"))
(when (featurep! +present)
  (package! centered-window
    :recipe (:host github :repo "anler/centered-window-mode")
    :pin "f50859941ab5c7cbeaee410f2d38716252b552ac")
  (package! org-tree-slide :pin "9d2ba1df456d8d7c6372c8c294dbe3ee81540b33")
  (package! org-re-reveal :pin "ee712db65782ddc2bffe19c60cdc40b72ce56769")
  (package! revealjs
    :recipe (:host github :repo "hakimel/reveal.js"
             :files ("css" "dist" "js" "plugin"))
    :pin "abe9abbed7c68d1e73f5befb6cf256b71d77e769"))
(cond
 ((featurep! +roam)
  (package! org-roam
    :recipe (:host github :repo "org-roam/org-roam-v1")
    :pin "946a879a4a18756a0508afba1e0b0fe070c6a8b4"))
 ((featurep! +roam2)
  (package! org-roam
    ;; FIXME A :recipe isn't strictly necessary, but without it, our package
    ;;       bumper fails to distinguish between org-roam v1 and v2.
    :recipe (:host github :repo "org-roam/org-roam")
    :pin "1795039ab93ef19611dbb3fca21c4211c4e655a9")))

;;; Babel
(package! ob-async :pin "9aac486073f5c356ada20e716571be33a350a982")
(when (featurep! :lang crystal)
  (package! ob-crystal :pin "d84c1adee4b269cdba06a97caedb8071561a09af"))
(when (featurep! :lang elixir)
  (package! ob-elixir :pin "8990a8178b2f7bd93504a9ab136622aab6e82e32"))
(when (featurep! :lang go)
  (package! ob-go :pin "2067ed55f4c1d33a43cb3f6948609d240a8915f5"))
(when (featurep! :lang hy)
  (package! ob-hy :pin "a42ecaf440adc03e279afe43ee5ef6093ddd542a"))
(when (featurep! :lang nim)
  (package! ob-nim :pin "6fd060a3ecd38be37e4ec2261cd65760a3c35a91"))
(when (featurep! :lang racket)
  (package! ob-racket
    :recipe (:host github :repo "DEADB17/ob-racket")
    :pin "d8fd51bddb019b0eb68755255f88fc800cfe03cb"))
(when (featurep! :lang rest)
  (package! ob-restclient :pin "bfbc4d8e8a348c140f9328542daf5d979f0993e2"))
(when (featurep! :lang scala)
  (package! ob-ammonite :pin "39937dff395e70aff76a4224fa49cf2ec6c57cca"))

;;; Export
(when (featurep! +pandoc)
  (package! ox-pandoc :pin "aa37dc7e94213d4ebedb85c384c1ba35007da18e"))
(when (featurep! +hugo)
  (package! ox-hugo
    :recipe (:host github :repo "kaushalmodi/ox-hugo" :nonrecursive t)
    :pin "1b8f2627cd63ac21b84c5abe3d5b607bc778670a"))
(when (featurep! :lang rst)
  (package! ox-rst :pin "99fa790da55b57a3f2e9aa187493ba434a64250e"))
