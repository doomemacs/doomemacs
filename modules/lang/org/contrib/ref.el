;;; lang/org/contrib/ref.el -*- lexical-binding: t; -*-
;;;###if (featurep! :lang latex)


(use-package! org-ref
  :init
  (setq org-ref-bibliography-notes org-directory
        org-ref-default-bibliography '(expand-file-name "library.bib" org-directory)
        org-ref-pdf-directory "~/Documents/Papers"))
