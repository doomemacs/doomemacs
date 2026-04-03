;; -*- no-byte-compile: t; -*-
;;; lang/ess/packages.el

(package! ess :pin "4e112590d1c13cfe464ca7de77837f1b956e4a9f")
(package! ess-view-data :pin "7dcbd23d4cef2030753d16e1ca1811d3466484e7")
(package! polymode :pin "4604f55cc020c75562526fb76b723e5e242c97c0")
(package! poly-R :pin "fee0b6e99943fa49ca5ba8ae1a97cbed5ed51946")
(package! quarto-mode :pin "a7b974f7d22ef939eaed8b9919434bcf20b1438f")

(when (modulep! +stan)
  (package! stan-mode :pin "2bfd1484e1a99f9971b1a8aa1b587cdca411ab55")
  (package! eldoc-stan :pin "2bfd1484e1a99f9971b1a8aa1b587cdca411ab55")
  (when (modulep! :completion company)
    (package! company-stan :pin "2bfd1484e1a99f9971b1a8aa1b587cdca411ab55"))
  (when (modulep! :checkers syntax -flymake)
    (package! flycheck-stan :pin "2bfd1484e1a99f9971b1a8aa1b587cdca411ab55")))

(when (< emacs-major-version 29)
  ;; See emacs-ess/ESS#1193
  (package! xterm-color :pin "86fab1d247eb5ebe6b40fa5073a70dfa487cd465"))
