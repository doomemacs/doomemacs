;; -*- no-byte-compile: t; -*-
;;; lang/latex/packages.el

(package! auctex
  :recipe (:files ("*.el" "*.info" "dir"
                   "doc" "etc" "images" "latex" "style"))
  :pin "86b2397abdc20a638e5751251026727bc6282022")
(package! adaptive-wrap :pin "70005d2012ab57c20be03c05aebd49318fe49c99")
(package! latex-preview-pane :pin "5297668a89996b50b2b62f99cba01cc544dbed2e")
(when (modulep! :editor evil +everywhere)
  (package! evil-tex :pin "2a3177c818f106e6c11032ac261f8691f5e11f74"))

;; Optional module features.

(when (modulep! +latexmk)
  (package! auctex-latexmk :pin "b00a95e6b34c94987fda5a57c20cfe2f064b1c7a"))

(when (modulep! +cdlatex)
  (package! cdlatex :pin "33770dec73138909714711b05a63e79da5a19ccd"))

;; Features according to other user selected options.

(when (modulep! :completion company)
  (package! company-auctex :pin "9400a2ec7459dde8cbf1a5d50dfee4e300ed7e18")
  (package! company-reftex :pin "42eb98c6504e65989635d95ab81b65b9d5798e76")
  (package! company-math :pin "3eb006874e309ff4076d947fcbd61bb6806aa508"))
