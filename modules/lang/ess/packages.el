;; -*- no-byte-compile: t; -*-
;;; lang/ess/packages.el

(package! ess :pin "8b4664e4a76a573b76ab3cf7a467d5e20fd5a6de")
(package! ess-R-data-view :pin "d6e98d3ae1e2a2ea39a56eebcdb73e99d29562e9")
(package! polymode :pin "15b6c1e94a450a65a0e32096855c31f4390a3963")
(package! poly-R :pin "e4a39caaf48e1c2e5afab3865644267b10610537")

(when (modulep! +stan)
  (package! stan-mode :pin "150bbbe5fd3ad2b5a3dbfba9d291e66eeea1a581")
  (package! eldoc-stan :pin "150bbbe5fd3ad2b5a3dbfba9d291e66eeea1a581")
  (when (modulep! :completion company)
    (package! company-stan :pin "150bbbe5fd3ad2b5a3dbfba9d291e66eeea1a581"))
  (when (modulep! :checkers syntax -flymake)
    (package! flycheck-stan :pin "150bbbe5fd3ad2b5a3dbfba9d291e66eeea1a581")))
