;; -*- no-byte-compile: t; -*-
;;; lang/latex/packages.el

(package! auctex :pin "59e251c4c82d878e605a13aec3752e544a99f21a")
(package! adaptive-wrap :pin "319db649fb083db3ad07b4c71ee6c9429497043b")
(package! latex-preview-pane :pin "5297668a89996b50b2b62f99cba01cc544dbed2e")
(when (featurep! :editor evil +everywhere)
  (package! evil-tex :pin "a3b6875d4027c675cf490622026a15a12fe1a911"))

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
