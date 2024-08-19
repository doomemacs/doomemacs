;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(when (modulep! :completion ivy)
  (package! bibtex-completion :pin "8b71b4f5ce62eeaf18067f57faaddc06449fbe1c")
  (package! ivy-bibtex :pin "8b71b4f5ce62eeaf18067f57faaddc06449fbe1c"))
(when (modulep! :completion helm)
  (package! bibtex-completion :pin "8b71b4f5ce62eeaf18067f57faaddc06449fbe1c")
  (package! helm-bibtex :pin "8b71b4f5ce62eeaf18067f57faaddc06449fbe1c"))
(when (modulep! :completion vertico)
  (package! citar :pin "07d2a63c99fe35cbd468f8e6a322de05f1a29469")
  (package! citar-embark :pin "07d2a63c99fe35cbd468f8e6a322de05f1a29469")
  (when (modulep! :lang org +roam)
    (package! citar-org-roam :pin "82d47b5df1926627f56a09055c69b49b31cbbb9f")))

(package! parsebib :pin "ace9df707108b17759c004c7387655277122d4c1")
(package! citeproc :pin "54184baaff555b5c7993d566d75dd04ed485b5c0")
