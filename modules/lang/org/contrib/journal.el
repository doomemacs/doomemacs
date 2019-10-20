;;; lang/org/contrib/journal.el -*- lexical-binding: t; -*-
;;;###if (featurep! +journal)

(map! :after org-journal
      :localleader
      (:map org-journal-search-mode-map
        "n" #'org-journal-search-next
        "p" #'org-journal-search-prev)
      (:map org-journal-mode-map
        "n" #'org-journal-open-next-entry
        "p" #'org-journal-open-previous-entry))
