;; -*- no-byte-compile: t; -*-
;;; lang/latex/packages.el

(package! auctex :pin "ea410dce4f24908b46649b77a4881f7454a9de04")
(package! adaptive-wrap :pin "0d5b4a07de76d87dd64333a566a8a0a845f2b9f0")
(package! latex-preview-pane :pin "5297668a89996b50b2b62f99cba01cc544dbed2e")
(when (featurep! :editor evil +everywhere)
  (package! evil-tex :pin "87445d4d2339436179e792609bfbff0eaf056a9c"))

;; Optional module features:

(when (featurep! +latexmk)
  (package! auctex-latexmk :pin "4d353522650d7685acbf1d38f7dbc504f734bd84"))

(when (featurep! +cdlatex)
  (package! cdlatex :pin "614a8d94f67cdc1eeef8371f7b6b90aef8a78158"))

;; Features according to other user selected options

(when (featurep! :completion company)
  (package! company-auctex :pin "9400a2ec7459dde8cbf1a5d50dfee4e300ed7e18")
  (package! company-reftex :pin "42eb98c6504e65989635d95ab81b65b9d5798e76")
  (package! company-math :pin "a796053590012e6a15c8b527b521ffc15d137bd0"))
