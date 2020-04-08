;;; lang/org/contrib/journal.el -*- lexical-binding: t; -*-
;;;###if (featurep! +journal)

;; HACK org-journal does some file-path magic at load time that creates
;;      duplicate `auto-mode-alist' entries, so we suppress it for now, so we
;;      can do it properly later.
(advice-add #'org-journal-update-auto-mode-alist :override #'ignore)

(after! org-journal
  (setq org-journal-dir (expand-file-name "journal/" org-directory)
        org-journal-cache-file (concat doom-cache-dir "org-journal")
        org-journal-file-pattern (org-journal-dir-and-format->regex
                                  org-journal-dir org-journal-file-format))

  (add-to-list 'auto-mode-alist (cons org-journal-file-pattern 'org-journal-mode))

  (map! (:map org-journal-mode-map
          :n "]f"  #'org-journal-open-next-entry
          :n "[f"  #'org-journal-open-previous-entry
          :n "C-n" #'org-journal-open-next-entry
          :n "C-p" #'org-journal-open-previous-entry)
        (:map org-journal-search-mode-map
          "C-n" #'org-journal-search-next
          "C-p" #'org-journal-search-previous)
        :localleader
        (:map org-journal-mode-map
          "c" #'org-journal-new-entry
          "d" #'org-journal-new-date-entry
          "n" #'org-journal-open-next-entry
          "p" #'org-journal-open-previous-entry
          (:prefix "s"
            "s" #'org-journal-search
            "f" #'org-journal-search-forever
            "F" #'org-journal-search-future
            "w" #'org-journal-search-calendar-week
            "m" #'org-journal-search-calendar-month
            "y" #'org-journal-search-calendar-year))
        (:map org-journal-search-mode-map
          "n" #'org-journal-search-next
          "p" #'org-journal-search-prev)))
