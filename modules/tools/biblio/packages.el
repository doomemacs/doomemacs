;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(when (modulep! :completion ivy)
  (package! bibtex-completion :pin "8ebf50d5bd368082d0b7ab79d26a52f372cdef98")
  (package! ivy-bibtex :pin "8ebf50d5bd368082d0b7ab79d26a52f372cdef98"))
(when (modulep! :completion helm)
  (package! bibtex-completion :pin "8ebf50d5bd368082d0b7ab79d26a52f372cdef98")
  (package! helm-bibtex :pin "8ebf50d5bd368082d0b7ab79d26a52f372cdef98"))
(when (modulep! :completion vertico)
  (package! citar :pin "5dac3d5bf287566f049b44465e415afb42f30ec3")
  (package! citar-embark :pin "5dac3d5bf287566f049b44465e415afb42f30ec3")
  (when (modulep! :lang org +roam2)
    (package! citar-org-roam :pin "86e9861a4931a4a0a4a7fc6c2be153ceb937a869")))

(package! parsebib :pin "1efca921cbb49380396df9d81308b32e55fc8b63")
(package! citeproc :pin "2623043b2546ee09a4bd86641870ca86332c0bcf")
