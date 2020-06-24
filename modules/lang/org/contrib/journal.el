;;; lang/org/contrib/journal.el -*- lexical-binding: t; -*-
;;;###if (featurep! +journal)

(use-package! org-journal
  :defer t
  :init
  ;; HACK `org-journal' adds a `magic-mode-alist' entry for detecting journal
  ;;      files, but this causes us lazy loaders a big problem: an unacceptable
  ;;      delay on the first file the user opens, because calling the autoloaded
  ;;      `org-journal-is-journal' pulls all of `org' with it. So, we replace it
  ;;      with our own, extra layer of heuristics.
  (add-to-list 'magic-mode-alist '(+org-journal-p . org-journal-mode))

  (defun +org-journal-p ()
    (when-let (buffer-file-name (buffer-file-name (buffer-base-buffer)))
      (if (or (featurep 'org-journal)
              (and (file-in-directory-p
                    buffer-file-name (expand-file-name org-journal-dir org-directory))
                   (delq! '+org-journal-p magic-mode-alist 'assq)
                   (require 'org-journal nil t)))
          (org-journal-is-journal))))

  ;; `org-journal-dir' defaults to "~/Documents/journal/", which is an odd
  ;; default, so we change it to {org-directory}/journal (we expand it after
  ;; org-journal is loaded).
  (setq org-journal-dir "journal/"
        org-journal-cache-file (concat doom-cache-dir "org-journal"))

  :config
  ;; Remove the orginal journal file detector and rely on `+org-journal-p'
  ;; instead, to avoid loading org-journal until the last possible moment.
  (setq magic-mode-alist (assq-delete-all 'org-journal-is-journal magic-mode-alist))

  ;; `org-journal' can't deal with symlinks, so resolve them here.
  (setq org-journal-dir (expand-file-name org-journal-dir org-directory)
        ;; Doom opts for an "open in a popup or here" strategy as a default.
        ;; Open in "other window" is less predictable, and can replace a window
        ;; we wanted to keep visible.
        org-journal-find-file #'find-file)
  
  ;; Setup carryover to include all configured TODO states.
  (setq org-journal-carryover-items  "TODO=\"TODO\"|TODO=\"PROJ\"|TODO=\"STRT\"|TODO=\"WAIT\"|TODO=\"HOLD\"")


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
