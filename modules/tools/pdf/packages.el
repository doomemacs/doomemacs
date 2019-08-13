;; -*- no-byte-compile: t; -*-
;;; tools/pdf/packages.el

(package! pdf-tools)

(when (featurep! +note)
  (package! org-pdftools :recipe (:host github :repo "fuxialexander/org-pdftools" :files ("*")))
  (package! org-noter :recipe (:host github :repo "fuxialexander/org-noter" :branch "pdf-notes-booster" :files ("*"))))
