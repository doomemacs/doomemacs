;; -*- no-byte-compile: t; -*-
;;; lang/ess/packages.el

(package! ess :pin "7b9123669cccb26f346624fc82f7e2ff74b5dc62")
(package! ess-view-data :pin "5ec1c7206f1431c7b24f0990497ecc7e0fb33939")
(package! polymode :pin "25ba9463a443f0e904147138f226284e437248d3")
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
  (package! xterm-color :pin "ce82e87ea3d277c7e4fc48ce390d540fbd78f6d1"))
