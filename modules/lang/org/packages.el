;; -*- no-byte-compile: t; -*-
;;; lang/org/packages.el

(package! org
  :recipe (:host github
           ;; REVIEW: I intentionally avoid git.savannah.gnu.org because of SSL
           ;;   issues (see #5655), uptime issues, download time, and lack of
           ;;   shallow clone support.
           :repo "emacs-straight/org-mode"
           :files (:defaults "etc")
           :depth 1
           ;; HACK: Org has a post-install step that generates org-version.el
           ;;   and org-loaddefs.el, but Straight doesn't invoke this step, and
           ;;   the former doesn't work if the Org repo is a shallow clone.
           ;;   Rather than impose the network burden of a full clone (and other
           ;;   redundant work in Org's makefile), I'd rather fake these files
           ;;   instead. Besides, Straight already produces a org-autoloads.el,
           ;;   so org-loaddefs.el isn't needed.
           :build t
           :pre-build
           (progn
             (with-temp-file "org-loaddefs.el")
             (with-temp-file "org-version.el"
               (let ((version
                      (with-temp-buffer
                        (insert-file-contents "lisp/org.el" nil 0 1024)
                        (if (re-search-forward "^;; Version: \\([^\n-]+\\)" nil t)
                            (match-string-no-properties 1)
                          "Unknown"))))
                 (insert (format "(defun org-release () %S)\n" version)
                         (format "(defun org-git-version (&rest _) \"%s-??-%s\")\n"
                                 version (cdr (doom-call-process "git" "rev-parse" "--short" "HEAD")))
                         "(provide 'org-version)\n")))))
  :pin "2f5add467c0c20c9f2427cc77597c9aa1b742a75")  ; release_9.7.22
(package! org-contrib
  :recipe (:host github
           :repo "emacsmirror/org-contrib")
  :pin "f22bdd6a580953a10df1f218e65cf250ac0ae87c")

(package! avy)
(package! htmlize :pin "8e3841c837b4b78bd72ad7f0436e919f39315a46")
(package! ox-clip :pin "a549cc8e1747beb6b7e567ffac27e31ba45cb8e8")
(package! toc-org :pin "6d3ae0fc47ce79b1ea06cabe21a3c596395409cd")
(package! org-cliplink :pin "13e0940b65d22bec34e2de4bc8cba1412a7abfbc")

;; TODO Adjust when this is added to GNU ELPA
(when (modulep! +contacts)
  (package! org-contacts
    :recipe (:host github :repo "doomelpa/org-contacts")
    :pin "f0a430442b2ae60035dcd74fc6a76299875694f3"))

(when (and (featurep :system 'macos)
           (modulep! :os macos))
  (package! org-mac-link :pin "e30171a6e98db90787ab8a23b3a7dc4fd13b10f9"))

(when (modulep! +passwords)
  (package! org-passwords
    :pin "61584aa701defcc0c435d3e7552916235cb655a6"
    :recipe (:host github
             :repo "alfaromurillo/org-passwords.el")))

(when (modulep! :editor evil +everywhere)
  (package! evil-org
    :recipe (:host github :repo "doomelpa/evil-org-mode")
    :pin "06518c65ff4f7aea2ea51149d701549dcbccce5d"))
(when (modulep! :tools pdf)
  (package! org-pdftools :pin "5613b7ae561e0af199f25aacc0a9c34c16638408"))
(when (modulep! :tools magit)
  (package! orgit :pin "3b6b34bd1109adcad2c6dca920617fc91982547f")
  (when (modulep! :tools magit +forge)
    (package! orgit-forge :pin "2718a6aaf0f64cb52c64c419053fbc80eb358c8d")))
(when (modulep! +brain)
  (package! org-brain :pin "2bad7732aae1a3051e2a14de2e30f970bbe43c25"))
(when (modulep! +dragndrop)
  (package! org-download :pin "c8be2611786d1d8d666b7b4f73582de1093f25ac"))
(when (modulep! +gnuplot)
  (package! gnuplot :pin "4c6b18f71ff7604e2640033207f5a882ddce78af")
  (package! gnuplot-mode :pin "601f6392986f0cba332c87678d31ae0d0a496ce7"))
(when (modulep! +jupyter)
  (package! jupyter :pin "db8a9e233a010a61063f34220821ec76157a2d84"))
(when (modulep! +journal)
  (package! org-journal :pin "115b9c035406f296666ccbeee12c6f9a7ff0f837"))
(when (modulep! +noter)
  (package! org-noter :pin "691efc3ed4a2828d791a148e53851365c2eb380f"))
(when (modulep! +pomodoro)
  (package! org-pomodoro :pin "3f5bcfb80d61556d35fc29e5ddb09750df962cc6"))
(when (modulep! +pretty)
  (package! org-appear :pin "32ee50f8fdfa449bbc235617549c1bccb503cb09")
  (package! org-superstar :pin "54c81c27dde2a6dc461bb064e79a8b2089093a2e")
  (package! org-fancy-priorities :pin "7f677c6c14ecf05eab8e0efbfe7f1b00ae68eb1d"))
(when (modulep! +present)
  (package! centered-window
    :recipe (:host github :repo "anler/centered-window-mode")
    :pin "80965f6c6afe8d918481433984b493de72af5399")
  (package! org-tree-slide :pin "e2599a106a26ce5511095e23df4ea04be6687a8a")
  (package! org-re-reveal :pin "88e9d9e679e75e40acd93566a27a342437419992")
  (package! revealjs
    :recipe (:host github :repo "hakimel/reveal.js"
             :files ("css" "dist" "js" "plugin"))
    :pin "2059d388f79fd2a8d3b451141e246ab9862105d7"))
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
    :pin "0037daaf3eb2d1c3a1c215efb4d38a32db140224")
  (when (< emacs-major-version 29)
    ;; HACK: Needed until org-roam/org-roam#2485 is resolved.
    (package! emacsql :pin "491105a01f58bf0b346cbc0254766c6800b229a2"))))

;;; Babel
(package! ob-async :pin "9aac486073f5c356ada20e716571be33a350a982")
(when (modulep! :lang clojure)
  (package! ob-clojure-literate
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
    :recipe (:host github :repo "doomelpa/ob-php")
    :pin "6ebf7799e9ded1d5114094f46785960a50000614"))
(when (modulep! :lang racket)
  (package! ob-racket
    :recipe (:host github :repo "DEADB17/ob-racket")
    :pin "d8fd51bddb019b0eb68755255f88fc800cfe03cb"))
(when (modulep! :lang rest)
  (package! ob-restclient :pin "8183f8af08838854cf145ca4855b373f3e7c44b0"))
(when (modulep! :lang scala)
  (package! ob-ammonite :pin "39937dff395e70aff76a4224fa49cf2ec6c57cca"))

;;; Export
(when (modulep! +pandoc)
  (package! ox-pandoc :pin "34e6ea97b586e20529d07158a73af3cf33cdd1d5"))
(when (modulep! +hugo)
  (package! ox-hugo
    :recipe (:host github :repo "kaushalmodi/ox-hugo" :nonrecursive t)
    :pin "e3365cb4e65c1853d8838b863a21546bbd9e0990"))
(when (modulep! :lang rst)
  (package! ox-rst :pin "99fa790da55b57a3f2e9aa187493ba434a64250e"))
