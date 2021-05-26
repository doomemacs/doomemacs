;; -*- no-byte-compile: t; -*-
;;; lang/org/packages.el

(package! org-mode
  :recipe (:host github
           ;; Install cutting-edge version of org-mode, and from a mirror,
           ;; because code.orgmode.org runs on a potato.
           :repo "emacs-straight/org-mode"
           :files ("*.el" "lisp/*.el" "contrib/lisp/*.el" "contrib/scripts")
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
                     "(provide 'org-version)\n"))
           ;; Prevents built-in Org from sneaking into the byte-compilation of
           ;; `org-plus-contrib', and inform other packages that `org-mode'
           ;; satisfies the `org' dependency: raxod502/straight.el#352
           :includes (org org-plus-contrib))
  :pin "7a62a4d3251a512069aa06b0082529d61d22de26")

(package! avy)
(package! htmlize :pin "49205105898ba8993b5253beec55d8bddd820a70")
(package! org-yt
  :recipe (:host github :repo "TobiasZawada/org-yt")
  :pin "40cc1ac76d741055cbefa13860d9f070a7ade001")
(package! ox-clip :pin "2095537695135c7f1bc19db043925eb7d482907b")
(package! toc-org :pin "c4c61c5a382f94a3a4537e254243006dec2dcca4")
(package! org-cliplink :pin "13e0940b65d22bec34e2de4bc8cba1412a7abfbc")

(when (featurep! :editor evil +everywhere)
  (package! evil-org
    :recipe (:host github :repo "hlissner/evil-org-mode")
    :pin "a9706da260c45b98601bcd72b1d2c0a24a017700"))
(when (featurep! :tools pdf)
  (package! org-pdftools :pin "a5b61bca3f8c91b0859bb0df1a929f9a31a57b99"))
(when (featurep! :tools magit)
  (package! orgit :pin "609fd0ccfb5268704b5bc7d7ac1014d4960b9707")
  (when (featurep! :tools magit +forge)
    (package! orgit-forge :pin "ea2a1cf9d337901b413e9df258b8e07af55c00f6")))
(when (featurep! +brain)
  (package! org-brain :pin "e9b9b3e5bb3c63cecb1367df49205c346d9c050a"))
(when (featurep! +dragndrop)
  (package! org-download :pin "947ca223643d28e189480e607df68449c15786cb"))
(when (featurep! +gnuplot)
  (package! gnuplot :pin "116cad8e09024223f97e81b0a4503cef20de9bf5")
  (package! gnuplot-mode :pin "601f6392986f0cba332c87678d31ae0d0a496ce7"))
(when (featurep! +ipython) ; DEPRECATED
  (package! ob-ipython :pin "7147455230841744fb5b95dcbe03320313a77124"))
(when (featurep! +jupyter)
  (package! jupyter :pin "6ce8d01e3a550a3268b415bf9d9b635d4dba5940"))
(when (featurep! +journal)
  (package! org-journal :pin "043bb9e26f75066dc1787cdc9265daca7a14dd4e"))
(when (featurep! +noter)
  (package! org-noter :pin "9ead81d42dd4dd5074782d239b2efddf9b8b7b3d"))
(when (featurep! +pomodoro)
  (package! org-pomodoro :pin "aa07c11318f91219336197e62c47bc7a3d090479"))
(when (featurep! +pretty)
  (package! org-superstar :pin "9d64c42e5029910153ec74cb9b5747b074281140")
  (package! org-fancy-priorities :pin "819bb993b71e7253cefef7047306ab4e0f9d0a86"))
(when (featurep! +present)
  (package! centered-window
    :recipe (:host github :repo "anler/centered-window-mode")
    :pin "f50859941ab5c7cbeaee410f2d38716252b552ac")
  (package! org-tree-slide :pin "9d2ba1df456d8d7c6372c8c294dbe3ee81540b33")
  (package! org-re-reveal :pin "18a2456befcfda5f681b2b4041f3262f93e52cba")
  (package! revealjs
    :recipe (:host github :repo "hakimel/reveal.js"
             :files ("css" "dist" "js" "plugin"))
    :pin "cf8e64bd8504737912b39e4153390cffbf443ed7"))
(when (featurep! +roam)
  (package! org-roam :pin "8ad57b121831eda8d226faa14ff2ba7ab652849c"))

;;; Babel
(package! ob-async :pin "de1cd6c93242a4cb8773bbe115b7be3d4dd6b97e")
(when (featurep! :lang crystal)
  (package! ob-crystal :pin "d84c1adee4b269cdba06a97caedb8071561a09af"))
(when (featurep! :lang elixir)
  (package! ob-elixir :pin "8990a8178b2f7bd93504a9ab136622aab6e82e32"))
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
    :pin "02140a294a8d0d15ca42a1956af794fd7ec18140"))
(when (featurep! :lang rst)
  (package! ox-rst :pin "99fa790da55b57a3f2e9aa187493ba434a64250e"))
