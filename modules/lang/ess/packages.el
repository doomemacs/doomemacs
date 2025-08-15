;; -*- no-byte-compile: t; -*-
;;; lang/ess/packages.el

(package! ess :pin "d19efaae125a1a76840e53259eaa4880747f968f")
(package! ess-view-data :pin "dd6a85935bbee0f497d0e8731abdfa07150600b7")
(package! polymode :pin "74ba75d4bcfbea959ccc9080a95ab9ef759849f2")
(package! poly-R :pin "fee0b6e99943fa49ca5ba8ae1a97cbed5ed51946")
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
