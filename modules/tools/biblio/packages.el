;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(when (modulep! :completion ivy)
  (package! bibtex-completion :pin "6064e8625b2958f34d6d40312903a85c173b5261")
  (package! ivy-bibtex :pin "6064e8625b2958f34d6d40312903a85c173b5261"))
(when (modulep! :completion helm)
  (package! bibtex-completion :pin "6064e8625b2958f34d6d40312903a85c173b5261")
  (package! helm-bibtex :pin "6064e8625b2958f34d6d40312903a85c173b5261"))
(when (modulep! :completion vertico)
  (package! citar :pin "ce5e9644ed02cc1ed4a905e0436a1be8f8ccab57")
  (package! citar-embark :pin "ce5e9644ed02cc1ed4a905e0436a1be8f8ccab57")
  (when (modulep! :lang org +roam2)
    (package! citar-org-roam :pin "9750cfbbf330ab3d5b15066b65bd0a0fe7c296fb")))

(package! parsebib :pin "7bfde4e4679413424a9a9af099203d5c23e32cd2")
(package! citeproc :pin "54184baaff555b5c7993d566d75dd04ed485b5c0")
