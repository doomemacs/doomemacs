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
  :pin "bed47b437d8cde7a98bafdb07996e248b40f70e6")
(package! org-contrib
  :recipe (:host github
           :repo "emacsmirror/org-contrib")
  :pin "aa104c0bbc3113f6d3d167b20bd8d6bf6a285f0f")

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
    :pin "bd5093e46b1d873a579db21cfab75d306df948b2"
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
  (package! orgit :pin "5036c3db5897ce93cd895aa213072661a497a4e6")
  (when (modulep! :tools magit +forge)
    (package! orgit-forge :pin "3a54569dd02909119e15550552ce6acf7ec2dacd")))
(when (modulep! +brain)
  (package! org-brain :pin "46ca9f766322cff31279ecdf02251ff24a0e9431"))
(when (modulep! +dragndrop)
  (package! org-download :pin "19e166f0a8c539b4144cfbc614309d47a9b2a9b7"))
(when (modulep! +gnuplot)
  (package! gnuplot :pin "7138b139d2dca9683f1a81325c643b2744aa1ea3")
  (package! gnuplot-mode :pin "601f6392986f0cba332c87678d31ae0d0a496ce7"))
(when (modulep! +ipython) ; DEPRECATED
  (package! ob-ipython :pin "7147455230841744fb5b95dcbe03320313a77124"))
(when (modulep! +jupyter)
  (package! jupyter :pin "7d20c0aee2f9c896215f35232905b23532ef04c5"))
(when (modulep! +journal)
  (package! org-journal :pin "1ea9e3b672ed384922ede6af96598446f3691873"))
(when (modulep! +noter)
  (package! org-noter :pin "9ead81d42dd4dd5074782d239b2efddf9b8b7b3d"))
(when (modulep! +pomodoro)
  (package! org-pomodoro :pin "3f5bcfb80d61556d35fc29e5ddb09750df962cc6"))
(when (modulep! +pretty)
  (package! org-appear :pin "60ba267c5da336e75e603f8c7ab3f44e6f4e4dac")
  (package! org-superstar :pin "03be6c0a3081c46a59b108deb8479ee24a6d86c0")
  (package! org-fancy-priorities :pin "7f677c6c14ecf05eab8e0efbfe7f1b00ae68eb1d"))
(when (modulep! +present)
  (package! centered-window
    :recipe (:host github :repo "anler/centered-window-mode")
    :pin "80965f6c6afe8d918481433984b493de72af5399")
  (package! org-tree-slide :pin "d6529bc2df727d09014e0e56abf4f15a8e8fc20f")
  (package! org-re-reveal :pin "941aa4b94f4eb13cea23013126e84f335059dead")
  (package! revealjs
    :recipe (:host github :repo "hakimel/reveal.js"
             :files ("css" "dist" "js" "plugin"))
    :pin "f6f657b627f9703e32414d8d3f16fb49d41031cb"))
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
    :pin "d95d25615e69e7cc847641800c1886366336c97e")))

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
    :pin "ca3d4568da6c82ff32a8d289743b059d9f909c67"))
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
  (package! ox-pandoc :pin "f8eac5e5692fc44a4724ada43191e7c28a1ccf30"))
(when (modulep! +hugo)
  (package! ox-hugo
    :recipe (:host github :repo "kaushalmodi/ox-hugo" :nonrecursive t)
    :pin "e532fce4a4fbf9a7981bed07d1b9ec30a4d0a305"))
(when (modulep! :lang rst)
  (package! ox-rst :pin "99fa790da55b57a3f2e9aa187493ba434a64250e"))
