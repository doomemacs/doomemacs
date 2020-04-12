;;; lang/org/contrib/journal.el -*- lexical-binding: t; -*-
;;;###if (featurep! +journal)

(use-package! org-journal
  :mode ("/\\(?1:[0-9]\\{4\\}\\)\\(?2:[0-9][0-9]\\)\\(?3:[0-9][0-9]\\)\\(\\.gpg\\)?\\'"
         . org-journal-mode)
  :preface
  ;; HACK org-journal does some file-path magic at load time that creates
  ;;      duplicate and hard-coded `auto-mode-alist' entries, so we suppress it
  ;;      and use the more generalize regexp (above).
  (advice-add #'org-journal-update-auto-mode-alist :override #'ignore)
  ;; HACK `org-journal-dir' has is surrounded by setter and `auto-mode-alist'
  ;;      magic which makes its needlessly difficult to create an "overrideable"
  ;;      default for Doom users, so we set this to an empty string (anything
  ;;      else will throw an error) so we can detect it being changed later.
  (setq org-journal-dir ""
        org-journal-cache-file (concat doom-cache-dir "org-journal"))
  :config
  (when (string-empty-p org-journal-dir)
    (setq! org-journal-dir (expand-file-name "journal/" org-directory)))
  (setq org-journal-find-file #'find-file)

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
