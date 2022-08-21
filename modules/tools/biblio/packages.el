;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(when (modulep! :completion ivy)
  (package! bibtex-completion :pin "ce8c17690ddad73d01531084b282f221f8eb6669")
  (package! ivy-bibtex :pin "ce8c17690ddad73d01531084b282f221f8eb6669"))
(when (modulep! :completion helm)
  (package! bibtex-completion :pin "ce8c17690ddad73d01531084b282f221f8eb6669")
  (package! helm-bibtex :pin "ce8c17690ddad73d01531084b282f221f8eb6669"))
(when (modulep! :completion vertico)
  (package! citar :pin "146f2cb5a31d4968ec17f39f189e4ea131ccaf56")
  (package! citar-embark :pin "146f2cb5a31d4968ec17f39f189e4ea131ccaf56")
  (when (modulep! :lang org +roam2)
    (package! citar-org-roam :pin "4867479a5daaa8a86d918ad52bc7026213e38ce4")))

(package! parsebib :pin "175a1bdac1eabc7415116c8722795a1155e2d2c9")
(package! citeproc :pin "406bd9964f1ce531fc45beddcf9ccc44d3456129")
