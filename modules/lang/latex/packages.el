;; -*- no-byte-compile: t; -*-
;;; lang/latex/packages.el

(package! auctex :pin "6abf890a485b2ff734d8f87f38393f9b8f6bbbf6")
(package! adaptive-wrap :pin "1810c0ee8d827dd502ddeaae5bd759d4811fcbce")
(package! latex-preview-pane :pin "5297668a89996b50b2b62f99cba01cc544dbed2e")
(when (featurep! :editor evil +everywhere)
  (package! evil-tex :pin "bb01576e4cf9cc10c529621ced3ef3a7eb5ab376"))

;; Optional module features:

(when (featurep! +latexmk)
  (package! auctex-latexmk :pin "4d353522650d7685acbf1d38f7dbc504f734bd84"))

(when (featurep! +cdlatex)
  (package! cdlatex :pin "480387b39f6ddd9cd2a9511ecee064ad8e1dd324"))

;; Features according to other user selected options

(when (featurep! :completion company)
  (package! company-auctex :pin "9400a2ec7459dde8cbf1a5d50dfee4e300ed7e18")
  (package! company-reftex :pin "275ef708f08d3bf0eb30632148e5c6184eeaacdb")
  (package! company-math :pin "a796053590012e6a15c8b527b521ffc15d137bd0"))
