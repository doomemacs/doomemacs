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
  :pin "b7bc0ede67f3e2a477f4d6ad0c46a6d80bca5aea")  ; release_9.7.39
(package! org-contrib
  :recipe (:host github
           :repo "emacsmirror/org-contrib")
  :pin "90e1d6bd6288615233dae273f0525a43a9d8779d")

(package! avy)
(package! htmlize :pin "fa644880699adea3770504f913e6dddbec90c076")
(package! ox-clip :pin "a549cc8e1747beb6b7e567ffac27e31ba45cb8e8")
(package! toc-org :pin "6d3ae0fc47ce79b1ea06cabe21a3c596395409cd")
(package! org-cliplink :pin "13e0940b65d22bec34e2de4bc8cba1412a7abfbc")

;; TODO Adjust when this is added to GNU ELPA
(when (modulep! +contacts)
  (package! org-contacts
    :recipe (:host github :repo "doomelpa/org-contacts")
    :pin "b06a59736800865b8a7e8d6d45774169cb31528a"))

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
  (package! org-pdftools :pin "2b3357828a4c2dfba8f87c906d64035d8bf221f2"))
(when (modulep! :tools magit)
  (package! orgit :pin "24c8fe48c477d561c2ce1720223f8c5aec664f4e") ; v2.1.1
  (when (modulep! :tools magit +forge)
    (package! orgit-forge :pin "c2116b8701498bd11d8674065a5429d844985e46"))) ; v1.1.1
(when (modulep! +dragndrop)
  (package! org-download :pin "c8be2611786d1d8d666b7b4f73582de1093f25ac"))
(when (modulep! +gnuplot)
  (package! gnuplot :pin "4c6b18f71ff7604e2640033207f5a882ddce78af")
  (package! gnuplot-mode :pin "601f6392986f0cba332c87678d31ae0d0a496ce7"))
(when (modulep! +jupyter)
  (package! jupyter :pin "de89cbeca890db51ba84aee956658f89aaa0b642"))
(when (modulep! +journal)
  (package! org-journal :pin "831ecfd50a29057c239b9fa55ebc02d402a6d4a7"))
(when (modulep! +noter)
  (package! org-noter :pin "81765d267e51efd8b4f5b7276000332ba3eabbf5"))
(when (modulep! +pomodoro)
  (package! org-pomodoro :pin "3f5bcfb80d61556d35fc29e5ddb09750df962cc6"))
(when (modulep! +pretty)
  (package! org-modern :pin "9bbc44cc7e085dea24e96f0cc0332ed7fcf349ca")
  (package! org-appear :pin "32ee50f8fdfa449bbc235617549c1bccb503cb09"))
(when (modulep! +present)
  (package! centered-window
    :recipe (:host github :repo "nullvec/centered-window-mode")
    :pin "701f56cd1d2b68352d29914f05ca1b0037bb2595")
  (package! org-tree-slide :pin "e2599a106a26ce5511095e23df4ea04be6687a8a")
  (package! org-re-reveal :pin "8245facfdca168a728f3761d863af28ee05af171")
  (package! revealjs
    :recipe (:host github :repo "hakimel/reveal.js"
             :files ("css" "dist" "js" "plugin"))
    :pin "33bfe3b233f1a840cd70e834b609ec6f04494a40"))
(when (or (modulep! +roam)
          (modulep! +roam2))
  (package! org-roam :pin "c72702cf27891899ea6321fe6505ff04befaf43e"))

;;; Babel
(package! ob-async :pin "9aac486073f5c356ada20e716571be33a350a982")
(when (modulep! :lang clojure)
  (package! ob-clojure-literate
    :pin "18c3ea15b872a43e67c899a9914182c35b00b7ee"))
(when (modulep! :lang crystal)
  (package! ob-crystal :pin "d84c1adee4b269cdba06a97caedb8071561a09af"))
(when (modulep! :lang elixir)
  (package! ob-elixir :pin "8e5d2f3c7adb0d5acde390264fec94627aa7af31"))
(when (modulep! :lang fsharp)
  (package! ob-fsharp
    :recipe (:host github :repo "elken/ob-fsharp")
    :pin "a5bb40915a8b78fb3c5fc4b44ad00393e07e46a4"))
(when (modulep! :lang go)
  (package! ob-go :pin "c6c7c811fba278924888010ac1fa555297fe760a"))
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
  (package! ob-restclient :pin "94dd9cd98ff50717135ed5089afb378616faf11a"))
(when (modulep! :lang scala)
  (package! ob-ammonite :pin "39937dff395e70aff76a4224fa49cf2ec6c57cca"))

;;; Export
(when (modulep! +pandoc)
  (package! ox-pandoc :pin "1caeb56a4be26597319e7288edbc2cabada151b4"))
(when (modulep! +hugo)
  (package! ox-hugo
    :recipe (:host github :repo "kaushalmodi/ox-hugo" :nonrecursive t)
    :pin "b7dc44dc28911b9d8e3055a18deac16c3b560b03"))
(when (modulep! :lang rst)
  (package! ox-rst :pin "b73eff187eebac24b457688bfd27f09eff434860"))
