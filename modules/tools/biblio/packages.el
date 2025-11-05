;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(when (modulep! :completion ivy)
  (package! bibtex-completion :pin "6064e8625b2958f34d6d40312903a85c173b5261")
  (package! ivy-bibtex :pin "6064e8625b2958f34d6d40312903a85c173b5261"))
(when (modulep! :completion helm)
  (package! bibtex-completion :pin "6064e8625b2958f34d6d40312903a85c173b5261")
  (package! helm-bibtex :pin "6064e8625b2958f34d6d40312903a85c173b5261"))
(when (modulep! :completion vertico)
  (package! citar :pin "427432d490f116c6b10b7459593cff1b2a9ca9de")
  (package! citar-embark :pin "427432d490f116c6b10b7459593cff1b2a9ca9de")
  (when (or (modulep! :lang org +roam)
            (modulep! :lang org +roam2))
    (package! citar-org-roam :pin "9750cfbbf330ab3d5b15066b65bd0a0fe7c296fb")))

(package! parsebib :pin "4a9df6f1b4f37bbf4f8712eac99c8a25698f1c0e")
(package! citeproc :pin "a3d62ab8e40a75fcfc6e4c0c107e3137b4db6db8")
