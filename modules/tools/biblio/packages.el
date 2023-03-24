;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(when (modulep! :completion ivy)
  (package! bibtex-completion :pin "ef07adfeda1e25d891875cb9a11983d5e26fc36d")
  (package! ivy-bibtex :pin "ef07adfeda1e25d891875cb9a11983d5e26fc36d"))
(when (modulep! :completion helm)
  (package! bibtex-completion :pin "ef07adfeda1e25d891875cb9a11983d5e26fc36d")
  (package! helm-bibtex :pin "ef07adfeda1e25d891875cb9a11983d5e26fc36d"))
(when (modulep! :completion vertico)
  (package! citar :pin "2c0547db57f2fb30ff071d126b256287a4e9452c")
  (package! citar-embark :pin "2c0547db57f2fb30ff071d126b256287a4e9452c")
  (when (modulep! :lang org +roam2)
    (package! citar-org-roam :pin "761eed66782fdbb6d65749098caa42ba43e8441d")))

(package! parsebib :pin "ace9df707108b17759c004c7387655277122d4c1")
(package! citeproc :pin "290320fc579f886255f00d7268600df7fa5cc7e8")
