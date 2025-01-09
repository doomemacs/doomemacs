;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(when (modulep! :completion ivy)
  (package! bibtex-completion :pin "6064e8625b2958f34d6d40312903a85c173b5261")
  (package! ivy-bibtex :pin "6064e8625b2958f34d6d40312903a85c173b5261"))
(when (modulep! :completion helm)
  (package! bibtex-completion :pin "6064e8625b2958f34d6d40312903a85c173b5261")
  (package! helm-bibtex :pin "6064e8625b2958f34d6d40312903a85c173b5261"))
(when (modulep! :completion vertico)
  (package! citar :pin "2826996799ba9c6e318d51be264d8fec299484e4")
  (package! citar-embark :pin "2826996799ba9c6e318d51be264d8fec299484e4")
  (when (modulep! :lang org +roam2)
    (package! citar-org-roam :pin "ff38add0aa40ba2014a6ee28a293fc1b9180cefa")))

(package! parsebib :pin "f0e57a3606d615a54a05d82edb94058a0a6d92a9")
(package! citeproc :pin "54184baaff555b5c7993d566d75dd04ed485b5c0")
