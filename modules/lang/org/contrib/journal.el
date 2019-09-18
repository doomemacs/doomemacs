;;; lang/org/contrib/journal.el -*- lexical-binding: t; -*-

(use-package! org-journal
  :defer t
  :when (featurep! +journal)
  :config
  (map! :map org-journal-search-mode-map
        :localleader
        "n" #'org-journal-search-next
        "p" #'org-journal-search-prev)
  (map! :map org-journal-mode-map
        :localleader
        "n" #'org-journal-open-next-entry
        "p" #'org-journal-open-previous-entry))
