;; -*- no-byte-compile: t; -*-
;;; lang/ess/packages.el

(package! ess :pin "c72b911d704f81134b7518824f5eb58af0915bec")
(package! ess-R-data-view :pin "d6e98d3ae1e2a2ea39a56eebcdb73e99d29562e9")
(package! polymode :pin "74ba75d4bcfbea959ccc9080a95ab9ef759849f2")
(package! poly-R :pin "8024e852cfca642dea2045a41b2033baa2f1f9a5")

(when (modulep! +stan)
  (package! stan-mode :pin "150bbbe5fd3ad2b5a3dbfba9d291e66eeea1a581")
  (package! eldoc-stan :pin "150bbbe5fd3ad2b5a3dbfba9d291e66eeea1a581")
  (when (modulep! :completion company)
    (package! company-stan :pin "150bbbe5fd3ad2b5a3dbfba9d291e66eeea1a581"))
  (when (modulep! :checkers syntax -flymake)
    (package! flycheck-stan :pin "150bbbe5fd3ad2b5a3dbfba9d291e66eeea1a581")))
