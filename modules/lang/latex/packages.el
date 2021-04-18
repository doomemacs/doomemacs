;; -*- no-byte-compile: t; -*-
;;; lang/latex/packages.el

(package! auctex :pin "1472d1d231aeae463013d5e36307605157f84191")
(package! adaptive-wrap :pin "91e939b48a8129f696f45a7a3963fe09cbfa3a2d")
(package! latex-preview-pane :pin "5297668a89996b50b2b62f99cba01cc544dbed2e")
(when (featurep! :editor evil +everywhere)
  (package! evil-tex :pin "5f0d6fb11bce66d32c27c765e93557f6ca89cc7d"))

;; Optional module features:

(when (featurep! +latexmk)
  (package! auctex-latexmk :pin "4d353522650d7685acbf1d38f7dbc504f734bd84"))

(when (featurep! +cdlatex)
  (package! cdlatex :pin "adf96bab0bbf28f65c882c0874f1c14fdb216bd8"))

;; Features according to other user selected options

(when (featurep! :completion company)
  (package! company-auctex :pin "9400a2ec7459dde8cbf1a5d50dfee4e300ed7e18")
  (package! company-reftex :pin "291c283c8a015fd7cbaa99f836e1a721f1e2c832")
  (package! company-math :pin "a796053590012e6a15c8b527b521ffc15d137bd0"))
