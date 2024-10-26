;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(when (modulep! :completion ivy)
  (package! bibtex-completion :pin "8b71b4f5ce62eeaf18067f57faaddc06449fbe1c")
  (package! ivy-bibtex :pin "8b71b4f5ce62eeaf18067f57faaddc06449fbe1c"))
(when (modulep! :completion helm)
  (package! bibtex-completion :pin "8b71b4f5ce62eeaf18067f57faaddc06449fbe1c")
  (package! helm-bibtex :pin "8b71b4f5ce62eeaf18067f57faaddc06449fbe1c"))
(when (modulep! :completion vertico)
  (package! citar :pin "0f1786b7fee58452a3225e4b9b7c94176fff9b5a")
  (package! citar-embark :pin "0f1786b7fee58452a3225e4b9b7c94176fff9b5a")
  (when (modulep! :lang org +roam2)
    (package! citar-org-roam :pin "82d47b5df1926627f56a09055c69b49b31cbbb9f")))

(package! parsebib :pin "489f690f433bb44c6aa3b2f71db2645dcccdc213")
(package! citeproc :pin "54184baaff555b5c7993d566d75dd04ed485b5c0")
