;;; lang/org/contrib/noter.el -*- lexical-binding: t; -*-
;;;###if (featurep! +noter)


;; org-noter
(use-package! org-noter
  :preface
  ;; Allow the user to preempt this and set the document search path
  ;; If not set then use `org-directory'
  (defvar org-noter-notes-search-path nil)
  :init
  (map! :leader
        ;;; <leader> n --- notes
        (:prefix-map ("n" . "notes")
          :desc "Org Noter"          "r" #'org-noter
          :desc "Org Noter Skeleton" "c" #'org-noter-create-skeleton))
  :config
  (setq org-noter-auto-save-last-location t
        org-noter-separate-notes-from-heading t)
  (unless org-noter-notes-search-path
    (setq org-noter-notes-search-path org-directory)))
