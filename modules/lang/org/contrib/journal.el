;;; lang/org/contrib/journal.el -*- lexical-binding: t; -*-
;;;###if (featurep! +journal)

(after! org-journal
  (setq org-journal-dir (expand-file-name "journal/" org-directory)
        org-journal-file-pattern
        (expand-file-name "\\(?1:[0-9]\\{4\\}\\)\\(?2:[0-9][0-9]\\)\\(?3:[0-9][0-9]\\)\\'"
                          org-journal-dir))

  (map! :localleader
        (:map org-journal-search-mode-map
          "n" #'org-journal-search-next
          "p" #'org-journal-search-prev)
        (:map org-journal-mode-map
          "n" #'org-journal-open-next-entry
          "p" #'org-journal-open-previous-entry)))
