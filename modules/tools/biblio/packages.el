;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(when (modulep! :completion ivy)
  (package! bibtex-completion :pin "bf184cc311c9e1724f8b2eaf75b9e202c3aedd16")
  (package! ivy-bibtex :pin "bf184cc311c9e1724f8b2eaf75b9e202c3aedd16"))
(when (modulep! :completion helm)
  (package! bibtex-completion :pin "bf184cc311c9e1724f8b2eaf75b9e202c3aedd16")
  (package! helm-bibtex :pin "bf184cc311c9e1724f8b2eaf75b9e202c3aedd16"))
(when (modulep! :completion vertico)
  (package! citar :pin "885b86f6733fd70f42c32dd7791d3447f93db990")
  (package! citar-embark :pin "885b86f6733fd70f42c32dd7791d3447f93db990")
  (when (modulep! :lang org +roam2)
    (package! citar-org-roam :pin "7d67dccf80065a88cb86ce9a8274383a9e8269c1")))

(package! parsebib :pin "ace9df707108b17759c004c7387655277122d4c1")
(package! citeproc :pin "c61c98b9d230ea28b2ca49498134803e1f8ea526")
