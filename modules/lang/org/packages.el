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
  :pin "8b15a0d0b48a0e3ce09be0d208d74a01743cbbe0")  ; release_9.7.34
(package! org-contrib
  :recipe (:host github
           :repo "emacsmirror/org-contrib")
  :pin "f1f6b6ec812803ff99693255555a82960fb3545a")

(package! avy)
(package! htmlize :pin "c9a8196a59973fabb3763b28069af9a4822a5260")
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
  (package! orgit :pin "8493c248081a9ed71ad6fd61e4d6b48c8a0039ec")
  (when (modulep! :tools magit +forge)
    (package! orgit-forge :pin "5a0dbe26012b2e7885895f80283ba8974a1e8b38")))
(when (modulep! +brain)
  (package! org-brain :pin "2bad7732aae1a3051e2a14de2e30f970bbe43c25"))
(when (modulep! +dragndrop)
  (package! org-download :pin "c8be2611786d1d8d666b7b4f73582de1093f25ac"))
(when (modulep! +gnuplot)
  (package! gnuplot :pin "4c6b18f71ff7604e2640033207f5a882ddce78af")
  (package! gnuplot-mode :pin "601f6392986f0cba332c87678d31ae0d0a496ce7"))
(when (modulep! +jupyter)
  (package! jupyter :pin "3615c2de16988c4dd9d1978bfa10ee3092e85b33"))
(when (modulep! +journal)
  (package! org-journal :pin "c72d7c75f8a05d1032250e307d35797ceee7e578"))
(when (modulep! +noter)
  (package! org-noter :pin "aafa08a49c4c3311d9b17864629aceeff02d33da"))
(when (modulep! +pomodoro)
  (package! org-pomodoro :pin "3f5bcfb80d61556d35fc29e5ddb09750df962cc6"))
(when (modulep! +pretty)
  (package! org-modern :pin "d5e1f5af65cce53113e017d319edaff25641e15b")
  (package! org-appear :pin "32ee50f8fdfa449bbc235617549c1bccb503cb09"))
(when (modulep! +present)
  (package! centered-window
    :recipe (:host github :repo "anler/centered-window-mode")
    :pin "80965f6c6afe8d918481433984b493de72af5399")
  (package! org-tree-slide :pin "e2599a106a26ce5511095e23df4ea04be6687a8a")
  (package! org-re-reveal :pin "4eb0f7147447c956231f5c178fa454b7cb76741b")
  (package! revealjs
    :recipe (:host github :repo "hakimel/reveal.js"
             :files ("css" "dist" "js" "plugin"))
    :pin "4cf184924d59e3d2b6552190c740ea5c7ab07981"))
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
    :pin "89dfaef38b6caa3027f20f96a551dc8f194ac533")
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
  (package! ox-pandoc :pin "5766c70b6db5a553829ccdcf52fcf3c6244e443d"))
(when (modulep! +hugo)
  (package! ox-hugo
    :recipe (:host github :repo "kaushalmodi/ox-hugo" :nonrecursive t)
    :pin "a907ea95145fd1abee608028796cd079a925eb02"))
(when (modulep! :lang rst)
  (package! ox-rst :pin "b73eff187eebac24b457688bfd27f09eff434860"))
