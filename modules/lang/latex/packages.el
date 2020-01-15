;; -*- no-byte-compile: t; -*-
;;; lang/latex/packages.el

(package! auctex :pin "fafa28d54276a73604e696b51c6a1a36d727d3fb")
(package! adaptive-wrap :pin "1810c0ee8d827dd502ddeaae5bd759d4811fcbce")
(package! latex-preview-pane :pin "5297668a89996b50b2b62f99cba01cc544dbed2e")

;; Optional module features:

(when (featurep! +latexmk)
  (package! auctex-latexmk :pin "4d353522650d7685acbf1d38f7dbc504f734bd84"))

;; Features according to other user selected options

(when (featurep! :completion company)
  (package! company-auctex :pin "48c42c58ce2f0e693301b0cb2d085055410c1b25")
  (package! company-reftex :pin "33935e96540201adab43f3a765d62289eba9e286")
  (package! company-math :pin "600e49449644f6835f9dc3501bc58461999e8ab9"))
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "d4471232be26793fbf56c0ac3690b5f537c378b9"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "d4471232be26793fbf56c0ac3690b5f537c378b9"))
