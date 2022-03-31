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
  :pin "971eb6885ec996c923e955730df3bafbdc244e54")
(package! org-contrib
  :recipe (:host github
           :repo "emacsmirror/org-contrib")
  :pin "17f3c514356430448627104e015f155008b45575")

(package! avy)
(package! htmlize :pin "dd27bc3f26efd728f2b1f01f9e4ac4f61f2ffbf9")
(package! org-yt
  :recipe (:host github :repo "TobiasZawada/org-yt")
  :pin "40cc1ac76d741055cbefa13860d9f070a7ade001")
(package! ox-clip :pin "ff117cf3c619eef12eccc0ccbfa3f11adb73ea68")
(package! toc-org :pin "bf2e4b358efbd860ecafe6e74776de0885d9d100")
(package! org-cliplink :pin "13e0940b65d22bec34e2de4bc8cba1412a7abfbc")

(when (featurep! :editor evil +everywhere)
  (package! evil-org
    :recipe (:host github :repo "hlissner/evil-org-mode")
    :pin "a9706da260c45b98601bcd72b1d2c0a24a017700"))
(when (featurep! :tools pdf)
  (package! org-pdftools :pin "967f48fb5038bba32915ee9da8dc4e8b10ba3376"))
(when (featurep! :tools magit)
  (package! orgit :pin "42b7f682b3e4e487ff209a44221a729921241133")
  (when (featurep! :tools magit +forge)
    (package! orgit-forge :pin "36e57a0359992e02312f453b8086512e77beb150")))
(when (featurep! +brain)
  (package! org-brain :pin "46ca9f766322cff31279ecdf02251ff24a0e9431"))
(when (featurep! +dragndrop)
  (package! org-download :pin "947ca223643d28e189480e607df68449c15786cb"))
(when (featurep! +gnuplot)
  (package! gnuplot :pin "57be3c7addec31e226a5a27aa553e996f9c684e3")
  (package! gnuplot-mode :pin "601f6392986f0cba332c87678d31ae0d0a496ce7"))
(when (featurep! +ipython) ; DEPRECATED
  (package! ob-ipython :pin "7147455230841744fb5b95dcbe03320313a77124"))
(when (featurep! +jupyter)
  (package! jupyter :pin "0a7055d7b12cf98723110415b08ee91869fa7d94"))
(when (featurep! +journal)
  (package! org-journal :pin "f121450610650c63aabf13afd0d2089e05fad2e4"))
(when (featurep! +noter)
  (package! org-noter :pin "9ead81d42dd4dd5074782d239b2efddf9b8b7b3d"))
(when (featurep! +pomodoro)
  (package! org-pomodoro :pin "3f5bcfb80d61556d35fc29e5ddb09750df962cc6"))
(when (featurep! +pretty)
  (package! org-appear :pin "ffbd742267ff81ba8433177fac5d7fe22b6d68a9")
  (package! org-superstar :pin "03be6c0a3081c46a59b108deb8479ee24a6d86c0")
  (package! org-fancy-priorities :pin "7f677c6c14ecf05eab8e0efbfe7f1b00ae68eb1d"))
(when (featurep! +present)
  (package! centered-window
    :recipe (:host github :repo "anler/centered-window-mode")
    :pin "80965f6c6afe8d918481433984b493de72af5399")
  (package! org-tree-slide :pin "3faa042393ebfe5699a3bffce775f039d7416ceb")
  (package! org-re-reveal :pin "e5bae22b9ef63a9fcfbbaa000e087703751eaad0")
  (package! revealjs
    :recipe (:host github :repo "hakimel/reveal.js"
             :files ("css" "dist" "js" "plugin"))
    :pin "e281b3234e7991283ce4dcca705dd9a6a9ebe5d2"))
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
    :pin "36152590ad1e8ffea86cb909e5ef818cbdb2a22d")))

;;; Babel
(package! ob-async :pin "9aac486073f5c356ada20e716571be33a350a982")
(when (featurep! :lang crystal)
  (package! ob-crystal :pin "d84c1adee4b269cdba06a97caedb8071561a09af"))
(when (featurep! :lang elixir)
  (package! ob-elixir :pin "8990a8178b2f7bd93504a9ab136622aab6e82e32"))
(when (featurep! :lang go)
  (package! ob-go :pin "2067ed55f4c1d33a43cb3f6948609d240a8915f5"))
(when (featurep! :lang graphql)
  (package! ob-graphql :pin "7c35419f9eec5dc44967cbcfa13c7135b9a96bfc"))
(when (featurep! :lang hy)
  (package! ob-hy :pin "a42ecaf440adc03e279afe43ee5ef6093ddd542a"))
(when (featurep! :lang nim)
  (package! ob-nim :pin "6fd060a3ecd38be37e4ec2261cd65760a3c35a91"))
(when (featurep! :lang racket)
  (package! ob-racket
    :recipe (:host github :repo "DEADB17/ob-racket")
    :pin "d8fd51bddb019b0eb68755255f88fc800cfe03cb"))
(when (featurep! :lang rest)
  (package! ob-restclient :pin "586f1fa07f76aaca13cb3f86945759f4b9fb8db7"))
(when (featurep! :lang scala)
  (package! ob-ammonite :pin "39937dff395e70aff76a4224fa49cf2ec6c57cca"))

;;; Export
(when (featurep! +pandoc)
  (package! ox-pandoc :pin "b2e43b936249de2a100afb4262698105c39ce289"))
(when (featurep! +hugo)
  (package! ox-hugo
    :recipe (:host github :repo "kaushalmodi/ox-hugo" :nonrecursive t)
    :pin "65e349b306b7fa27285f4c663e44bb36f6c8e653"))
(when (featurep! :lang rst)
  (package! ox-rst :pin "99fa790da55b57a3f2e9aa187493ba434a64250e"))
