;; -*- no-byte-compile: t; -*-
;;; lang/ess/packages.el

(package! ess :pin "a7ce81bb768d7cc410885711cf99bad0f8941ac3")
(package! ess-R-data-view :pin "d6e98d3ae1e2a2ea39a56eebcdb73e99d29562e9")
(package! polymode :pin "54888d6c15249503e1a66da7bd7761a9eda9b075")
(package! poly-R :pin "c42ff3a4d0da96ccb7f826dca5c6b2eb558a2ab5")

(when (featurep! +stan)
  (package! stan-mode :pin "9bb858b9f1314dcf1a5df23e39f9af522098276b")
  (package! eldoc-stan :pin "9bb858b9f1314dcf1a5df23e39f9af522098276b")
  (when (featurep! :completion company)
    (package! company-stan :pin "9bb858b9f1314dcf1a5df23e39f9af522098276b"))
  (when (featurep! :checkers syntax)
    (package! flycheck-stan :pin "9bb858b9f1314dcf1a5df23e39f9af522098276b")))
