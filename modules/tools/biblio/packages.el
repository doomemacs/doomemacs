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
    (package! citar-org-roam :pin "ff38add0aa40ba2014a6ee28a293fc1b9180cefa")))

(package! parsebib :pin "a25621930e67e267133b08698a72fa80a42edfc8")
(package! citeproc :pin "54184baaff555b5c7993d566d75dd04ed485b5c0")
