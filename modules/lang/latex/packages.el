;; -*- no-byte-compile: t; -*-
;;; lang/latex/packages.el

(package! auctex
  :recipe (:files ("*.el" "*.info" "dir"
                   "doc" "etc" "images" "latex" "style"))
  :pin "830e40a0639aedc6c362a3a01e160aaa8246bb99")
(package! adaptive-wrap :pin "0d5b4a07de76d87dd64333a566a8a0a845f2b9f0")
(package! latex-preview-pane :pin "5297668a89996b50b2b62f99cba01cc544dbed2e")
(when (featurep! :editor evil +everywhere)
  (package! evil-tex :pin "0fa85c3fc88d96621002b5a1b79efcc06776642f"))

;; Optional module features.

(when (featurep! +latexmk)
  (package! auctex-latexmk :pin "4d353522650d7685acbf1d38f7dbc504f734bd84"))

(when (featurep! +cdlatex)
  (package! cdlatex :pin "8e963c68531f75e459e8ebe7a34fd3ba9d3729a0"))

;; Features according to other user selected options.

(when (featurep! :completion company)
  (package! company-auctex :pin "9400a2ec7459dde8cbf1a5d50dfee4e300ed7e18")
  (package! company-reftex :pin "42eb98c6504e65989635d95ab81b65b9d5798e76")
  (package! company-math :pin "45778f5731c97a21a83e3b965cbde42018709afd"))
