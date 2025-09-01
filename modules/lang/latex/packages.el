;; -*- no-byte-compile: t; -*-
;;; lang/latex/packages.el

(package! auctex
  :recipe (:files ("*.el" "*.info" "dir"
                   "doc" "etc" "images" "latex" "style"))
  :pin "a2809e4d7068aaa7fa9cbd51214606393d4ab9eb")
(package! adaptive-wrap :pin "d75665b9c88e65f91dadd1e5880905bbdb7c09b7")
(package! auctex-cont-latexmk :pin "88c5f04d841c212d2b8331153e9e5c2767cb7197")
(when (modulep! :editor evil +everywhere)
  (package! evil-tex :pin "2a3177c818f106e6c11032ac261f8691f5e11f74"))

;; Optional module features.

(when (modulep! +cdlatex)
  (package! cdlatex :pin "fac070f0164ac9f5859cb4cccba7d29a65c337f3"))

;; Features according to other user selected options.

(when (modulep! :completion company)
  (package! company-auctex :pin "9400a2ec7459dde8cbf1a5d50dfee4e300ed7e18")
  (package! company-reftex :pin "42eb98c6504e65989635d95ab81b65b9d5798e76")
  (package! company-math :pin "3eb006874e309ff4076d947fcbd61bb6806aa508"))
