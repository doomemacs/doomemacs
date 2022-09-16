;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(when (modulep! :completion ivy)
  (package! bibtex-completion :pin "78f5931e1cc82e7ae2bcf0508cf31d0d1629a8dd")
  (package! ivy-bibtex :pin "78f5931e1cc82e7ae2bcf0508cf31d0d1629a8dd"))
(when (modulep! :completion helm)
  (package! bibtex-completion :pin "78f5931e1cc82e7ae2bcf0508cf31d0d1629a8dd")
  (package! helm-bibtex :pin "78f5931e1cc82e7ae2bcf0508cf31d0d1629a8dd"))
(when (modulep! :completion vertico)
  (package! citar :pin "4a302fcc405d9747ab74df0e95a01ea489fa9bdd")
  (package! citar-embark :pin "4a302fcc405d9747ab74df0e95a01ea489fa9bdd")
  (when (modulep! :lang org +roam2)
    (package! citar-org-roam :pin "27105d0a9578279560cd79cfad5871e7e603bc58")))

(package! parsebib :pin "1efca921cbb49380396df9d81308b32e55fc8b63")
(package! citeproc :pin "e705911a29abcfc49d4435c538e88835074e4ef4")
