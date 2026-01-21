;; -*- no-byte-compile: t; -*-
;;; lang/ess/packages.el

(package! ess :pin "f8c464dc1b017b5397f8ec9955f7856493af1179")
(package! ess-view-data :pin "7dcbd23d4cef2030753d16e1ca1811d3466484e7")
(package! polymode :pin "14b1fd8d2a183f11b123f62e02801dc1139da9c1")
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
  (package! xterm-color :pin "86fab1d247eb5ebe6b40fa5073a70dfa487cd465"))
