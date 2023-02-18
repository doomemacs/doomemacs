;; -*- no-byte-compile: t; -*-
;;; lang/org/packages.el

(package! org
  :recipe (:host github
           ;; REVIEW I intentionally avoid git.savannah.gnu.org because of SSL
           ;;   issues (see #5655), uptime issues, download time, and lack of
           ;;   shallow clone support.
           :repo "emacs-straight/org-mode"
           :files (:defaults "etc")
           :depth 1
           ;; HACK Org requires a post-install compilation step to generate a
           ;;   org-version.el with org-release and org-git-version functions,
           ;;   using a 'git describe ...' call.  This won't work in a sparse
           ;;   clone and I value smaller network burdens on users over
           ;;   non-essential variables so we fake it:
           :build t
           :pre-build
           (with-temp-file "org-version.el"
             (let ((version
                    (with-temp-buffer
                      (insert-file-contents (doom-path "lisp/org.el") nil 0 1024)
                      (if (re-search-forward "^;; Version: \\([^\n-]+\\)" nil t)
                          (match-string-no-properties 1)
                        "Unknown"))))
               (insert (format "(defun org-release () %S)\n" version)
                       (format "(defun org-git-version (&rest _) \"%s-??-%s\")\n"
                               version (cdr (doom-call-process "git" "rev-parse" "--short" "HEAD")))
                       "(provide 'org-version)\n"))))
  :pin "fe92a3cedba541482d5954eacb2b13e6f57a39c4")
(package! org-contrib
  :recipe (:host github
           :repo "emacsmirror/org-contrib")
  :pin "fff6c888065588527b1c1d7dd7e41c29ef767e17")

(package! avy)
(package! htmlize :pin "dd27bc3f26efd728f2b1f01f9e4ac4f61f2ffbf9")
(package! org-yt
  :recipe (:host github :repo "TobiasZawada/org-yt")
  :pin "40cc1ac76d741055cbefa13860d9f070a7ade001")
(package! ox-clip :pin "ff117cf3c619eef12eccc0ccbfa3f11adb73ea68")
(package! toc-org :pin "bf2e4b358efbd860ecafe6e74776de0885d9d100")
(package! org-cliplink :pin "13e0940b65d22bec34e2de4bc8cba1412a7abfbc")

;; TODO Adjust when this is added to GNU ELPA
(when (modulep! +contacts)
  (package! org-contacts
    :pin "bb4032eb12c20d34555a4e670f28696cf31a7b54"
    :recipe (:host nil
             :type git
             :repo "https://repo.or.cz/org-contacts.git")))

(when (and IS-MAC
           (modulep! :os macos))
  (package! org-mac-link :pin "0b18c1d070b9601cc65c40e902169e367e4348c9"))

(when (modulep! +passwords)
  (package! org-passwords
    :pin "61584aa701defcc0c435d3e7552916235cb655a6"
    :recipe (:host github
             :repo "alfaromurillo/org-passwords.el")))

(when (modulep! :editor evil +everywhere)
  (package! evil-org
    :recipe (:host github :repo "hlissner/evil-org-mode")
    :pin "a9706da260c45b98601bcd72b1d2c0a24a017700"))
(when (modulep! :tools pdf)
  (package! org-pdftools :pin "967f48fb5038bba32915ee9da8dc4e8b10ba3376"))
(when (modulep! :tools magit)
  (package! orgit :pin "fc40397f7b36fa513d41f3119b7430cb236de9dd")
  (when (modulep! :tools magit +forge)
    (package! orgit-forge :pin "6f424ff3a2df7d9c00c217011949c1367cc15227")))
(when (modulep! +brain)
  (package! org-brain :pin "2bad7732aae1a3051e2a14de2e30f970bbe43c25"))
(when (modulep! +dragndrop)
  (package! org-download :pin "19e166f0a8c539b4144cfbc614309d47a9b2a9b7"))
(when (modulep! +gnuplot)
  (package! gnuplot :pin "7138b139d2dca9683f1a81325c643b2744aa1ea3")
  (package! gnuplot-mode :pin "601f6392986f0cba332c87678d31ae0d0a496ce7"))
(when (modulep! +ipython) ; DEPRECATED
  (package! ob-ipython :pin "7147455230841744fb5b95dcbe03320313a77124"))
(when (modulep! +jupyter)
  (package! jupyter :pin "eebeef1293287075f65d27f5442bb151bf27d92a"))
(when (modulep! +journal)
  (package! org-journal :pin "2f220a06e3bf6bfa5cda00ebd44817a7b2fd7b76"))
(when (modulep! +noter)
  (package! org-noter :pin "9ead81d42dd4dd5074782d239b2efddf9b8b7b3d"))
(when (modulep! +pomodoro)
  (package! org-pomodoro :pin "3f5bcfb80d61556d35fc29e5ddb09750df962cc6"))
(when (modulep! +pretty)
  (package! org-appear :pin "eb9f9db40aa529fe4b977235d86494b115281d17")
  (package! org-superstar :pin "54c81c27dde2a6dc461bb064e79a8b2089093a2e")
  (package! org-fancy-priorities :pin "7f677c6c14ecf05eab8e0efbfe7f1b00ae68eb1d"))
(when (modulep! +present)
  (package! centered-window
    :recipe (:host github :repo "anler/centered-window-mode")
    :pin "80965f6c6afe8d918481433984b493de72af5399")
  (package! org-tree-slide :pin "d6529bc2df727d09014e0e56abf4f15a8e8fc20f")
  (package! org-re-reveal :pin "91cdd82c47b86990b5eb41fe34446a042194cc83")
  (package! revealjs
    :recipe (:host github :repo "hakimel/reveal.js"
             :files ("css" "dist" "js" "plugin"))
    :pin "7de6ccb65b98f657f757cb5610613ad9d4f3da2b"))
(cond
 ((modulep! +roam)
  (package! org-roam
    :recipe (:host github :repo "org-roam/org-roam-v1")
    :pin "946a879a4a18756a0508afba1e0b0fe070c6a8b4"))
 ((modulep! +roam2)
  (package! org-roam
    ;; FIXME A :recipe isn't strictly necessary, but without it, our package
    ;;       bumper fails to distinguish between org-roam v1 and v2.
    :recipe (:host github :repo "org-roam/org-roam")
    :pin "74422df546a515bc984c2f3d3a681c09d6f43916")))

;;; Babel
(package! ob-async :pin "9aac486073f5c356ada20e716571be33a350a982")
(when (modulep! :lang clojure)
  (package! ob-clojure-literate
    :recipe (:type git
             :host nil
             :repo "https://repo.or.cz/ob-clojure-literate.el.git")
    :pin "18c3ea15b872a43e67c899a9914182c35b00b7ee"))
(when (modulep! :lang crystal)
  (package! ob-crystal :pin "d84c1adee4b269cdba06a97caedb8071561a09af"))
(when (modulep! :lang elixir)
  (package! ob-elixir :pin "8990a8178b2f7bd93504a9ab136622aab6e82e32"))
(when (modulep! :lang fsharp)
  (package! ob-fsharp
    :recipe (:host github :repo "elken/ob-fsharp")
    :pin "a5bb40915a8b78fb3c5fc4b44ad00393e07e46a4"))
(when (modulep! :lang go)
  (package! ob-go :pin "2067ed55f4c1d33a43cb3f6948609d240a8915f5"))
(when (modulep! :lang graphql)
  (package! ob-graphql :pin "7c35419f9eec5dc44967cbcfa13c7135b9a96bfc"))
(when (modulep! :lang hy)
  (package! ob-hy :pin "a42ecaf440adc03e279afe43ee5ef6093ddd542a"))
(when (modulep! :lang nim)
  (package! ob-nim :pin "6fd060a3ecd38be37e4ec2261cd65760a3c35a91"))
(when (modulep! :lang php)
  (package! ob-php
    :recipe (:type git
             :host nil
             :repo "https://repo.or.cz/ob-php.git")
    :pin "6ebf7799e9ded1d5114094f46785960a50000614"))
(when (modulep! :lang racket)
  (package! ob-racket
    :recipe (:host github :repo "DEADB17/ob-racket")
    :pin "d8fd51bddb019b0eb68755255f88fc800cfe03cb"))
(when (modulep! :lang rest)
  (package! ob-restclient :pin "1b021ce1c67c97fa1aa4d2c0816edb7add129e48"))
(when (modulep! :lang scala)
  (package! ob-ammonite :pin "39937dff395e70aff76a4224fa49cf2ec6c57cca"))

;;; Export
(when (modulep! +pandoc)
  (package! ox-pandoc :pin "66c32cca4f6047dd7e0f77f10bd565a2d83d4729"))
(when (modulep! +hugo)
  (package! ox-hugo
    :recipe (:host github :repo "kaushalmodi/ox-hugo" :nonrecursive t)
    :pin "706b5f622d168bb8e0cf503e0525d2db9177c83e"))
(when (modulep! :lang rst)
  (package! ox-rst :pin "99fa790da55b57a3f2e9aa187493ba434a64250e"))
