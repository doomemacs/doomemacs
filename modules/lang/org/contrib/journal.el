;;; lang/org/contrib/journal.el -*- lexical-binding: t; -*-
;;;###if (featurep! +journal)

(use-package! org-journal
  :defer t
  :config
  (map! :map org-journal-search-mode-map
        :localleader
        "n" #'org-journal-search-next
        "p" #'org-journal-search-prev)
  (map! :map org-journal-mode-map
        :localleader
        "n" #'org-journal-open-next-entry
        "p" #'org-journal-open-previous-entry))
