;; -*- no-byte-compile: t; -*-
;;; lang/ess/packages.el

(package! ess :pin "39eba283000a7b0220303d7c5a4f3ee05efc1e9c")
(package! ess-R-data-view :pin "d6e98d3ae1e2a2ea39a56eebcdb73e99d29562e9")
(package! polymode :pin "2094c92403fe395dfb2b8b2521da1012a966e9ab")
(package! poly-R :pin "e4a39caaf48e1c2e5afab3865644267b10610537")

(when (featurep! +stan)
  (package! stan-mode :pin "150bbbe5fd3ad2b5a3dbfba9d291e66eeea1a581")
  (package! eldoc-stan :pin "150bbbe5fd3ad2b5a3dbfba9d291e66eeea1a581")
  (when (featurep! :completion company)
    (package! company-stan :pin "150bbbe5fd3ad2b5a3dbfba9d291e66eeea1a581"))
  (when (featurep! :checkers syntax)
    (package! flycheck-stan :pin "150bbbe5fd3ad2b5a3dbfba9d291e66eeea1a581")))
