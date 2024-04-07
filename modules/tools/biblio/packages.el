;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(when (modulep! :completion ivy)
  (package! bibtex-completion :pin "8b71b4f5ce62eeaf18067f57faaddc06449fbe1c")
  (package! ivy-bibtex :pin "8b71b4f5ce62eeaf18067f57faaddc06449fbe1c"))
(when (modulep! :completion helm)
  (package! bibtex-completion :pin "8b71b4f5ce62eeaf18067f57faaddc06449fbe1c")
  (package! helm-bibtex :pin "8b71b4f5ce62eeaf18067f57faaddc06449fbe1c"))
(when (modulep! :completion vertico)
  (package! citar :pin "885b86f6733fd70f42c32dd7791d3447f93db990")
  (package! citar-embark :pin "885b86f6733fd70f42c32dd7791d3447f93db990")
  (when (modulep! :lang org +roam2)
    (package! citar-org-roam :pin "82d47b5df1926627f56a09055c69b49b31cbbb9f")))

(package! parsebib :pin "ace9df707108b17759c004c7387655277122d4c1")
(package! citeproc :pin "44f90cb296766e03fffc28b7892521ab0e8709f1")
