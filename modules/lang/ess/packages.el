;; -*- no-byte-compile: t; -*-
;;; lang/ess/packages.el

(package! ess :pin "56f355acbd835a5a0d65c495e6a80dc01a36d556")
(package! ess-view-data :pin "dd6a85935bbee0f497d0e8731abdfa07150600b7")
(package! polymode :pin "74ba75d4bcfbea959ccc9080a95ab9ef759849f2")
(package! poly-R :pin "8024e852cfca642dea2045a41b2033baa2f1f9a5")
(package! quarto-mode :pin "a7b974f7d22ef939eaed8b9919434bcf20b1438f")

(when (modulep! +stan)
  (package! stan-mode :pin "150bbbe5fd3ad2b5a3dbfba9d291e66eeea1a581")
  (package! eldoc-stan :pin "150bbbe5fd3ad2b5a3dbfba9d291e66eeea1a581")
  (when (modulep! :completion company)
    (package! company-stan :pin "150bbbe5fd3ad2b5a3dbfba9d291e66eeea1a581"))
  (when (modulep! :checkers syntax -flymake)
    (package! flycheck-stan :pin "150bbbe5fd3ad2b5a3dbfba9d291e66eeea1a581")))

(when (< emacs-major-version 29)
  ;; See emacs-ess/ESS#1193
  (package! xterm-color :pin "2ad407c651e90fff2ea85d17bf074cee2c022912"))
