;;; lang/org/contrib/journal.el -*- lexical-binding: t; -*-
;;;###if (featurep! +journal)

(use-package! org-journal
  :hook (org-load . org-journal-update-auto-mode-alist)
  ;; This is necessary if the user decides opens a journal file directly, via
  ;; `find-file' or something, and not through org-journal's commands.
  :mode ("/[0-9]\\{8\\}\\.org\\(?:\\.gpg\\)?\\'" . org-journal-mode)
  :preface
  ;; Not using the .org file extension causes needless headache with file
  ;; detection for no compelling reason, so we make it the default, so
  ;; `org-journal' doesn't have to do all its `auto-mode-alist' magic.
  (defvar org-journal-file-format "%Y%m%d.org")

  ;; HACK `org-journal-dir' is surrounded with setters and `auto-mode-alist'
  ;;      magic which makes it difficult to create an better default for Doom
  ;;      users. We set this here so we can detect user-changes to it later.
  (setq org-journal-dir "journal/"
        org-journal-cache-file (concat doom-cache-dir "org-journal")
        ;; Doom opts for an "open in a popup or here" strategy as a default.
        ;; Open in "other window" is less predictable, and can replace a window
        ;; we wanted to keep visible.
        org-journal-find-file #'find-file)

  :config
  (when (or (equal org-journal-dir "journal/")
            (not (equal org-journal-file-format "%Y%m%d.org")))
    ;; HACK `org-journal' does some file-path magic at load time that creates
    ;;      several, duplicate and hard-coded `auto-mode-alist' entries, so get
    ;;      rid of them all here so we can create a one-true-entry right after.
    (setq auto-mode-alist (rassq-delete-all 'org-journal-mode auto-mode-alist))
    ;; ...By exploiting `org-journal-dir''s setter
    (setq! org-journal-dir (expand-file-name org-journal-dir org-directory)))

  (set-popup-rule! "^\\*Org-journal search" :select t :quit t)

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
