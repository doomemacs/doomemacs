;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(when (modulep! :completion ivy)
  (package! bibtex-completion :pin "6064e8625b2958f34d6d40312903a85c173b5261")
  (package! ivy-bibtex :pin "6064e8625b2958f34d6d40312903a85c173b5261"))
(when (modulep! :completion helm)
  (package! bibtex-completion :pin "6064e8625b2958f34d6d40312903a85c173b5261")
  (package! helm-bibtex :pin "6064e8625b2958f34d6d40312903a85c173b5261"))
(when (modulep! :completion vertico)
  (package! citar :pin "911a7d59c4ac94318fc6b6fb6f55840ad04482ad")
  (package! citar-embark :pin "911a7d59c4ac94318fc6b6fb6f55840ad04482ad")
  (when (or (modulep! :lang org +roam)
            (modulep! :lang org +roam2))
    (package! citar-org-roam :pin "9750cfbbf330ab3d5b15066b65bd0a0fe7c296fb")))

(package! parsebib :pin "5b837e0a5b91a69cc0e5086d8e4a71d6d86dac93")
(package! citeproc :pin "4bde999a41803fe519ea80eab8b813d53503eebd")
