;;; lang/org/contrib/journal.el -*- lexical-binding: t; -*-
;;;###if (featurep! +journal)

(use-package! org-journal
  :hook (org-mode . +org-journal-mode-maybe)
  :init
  ;; HACK org-journal does some file-path magic at load time that creates
  ;;      duplicate and hard-coded `auto-mode-alist' entries, so we suppress it
  ;;      and use the more generalize regexp (above).
  (advice-add #'org-journal-update-auto-mode-alist :override #'ignore)
  ;; Not using the .org file extension causes so much needless headache with
  ;; file detection, and for no compelling reason, so we make it the default, so
  ;; `org-journal' doesn't have to do all this silly magic.
  (setq org-journal-file-format "%Y%m%d.org")

  ;; HACK `org-journal-dir' has is surrounded by setter and `auto-mode-alist'
  ;;      magic which makes its needlessly difficult to create an "overrideable"
  ;;      default for Doom users, so we set this to an empty string (a
  ;;      non-string would throw an error) so we can detect changes to it later.
  (setq org-journal-dir ""
        org-journal-cache-file (concat doom-cache-dir "org-journal")
        ;; Doom opts for an "open in a popup or here" strategy as a default.
        ;; Open in "other window" is less consistent and harder to predict.
        org-journal-find-file #'find-file)

  :config
  ;; This is necessary if the user decides opens a journal file directly, via
  ;; `find-file' or something, and not through org-journal's commands.
  (defun +org-journal-mode-maybe ()
    "Activate `org-journal-mode', maybe."
    (and (eq major-mode 'org-mode)
         (stringp buffer-file-name)
         (stringp org-journal-file-pattern)
         (string-match-p org-journal-file-pattern buffer-file-name)
         (let ((org-mode-hook (remq '+org-journal-mode-maybe org-mode-hook)))
           (org-journal-mode))))

  (when (string-empty-p org-journal-dir)
    (setq org-journal-dir (expand-file-name "journal/" org-directory)))

  (advice-remove #'org-journal-update-auto-mode-alist #'ignore)
  (setq! org-journal-dir org-journal-dir)

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
