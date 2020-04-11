;;; lang/org/contrib/noter.el -*- lexical-binding: t; -*-
;;;###if (featurep! +noter)

(use-package! org-noter
  :defer t
  :preface
  ;; Allow the user to preempt this and set the document search path
  ;; If not set then use `org-directory'
  (defvar org-noter-notes-search-path nil)
  :config
  (setq org-noter-auto-save-last-location t
        org-noter-separate-notes-from-heading t)
  (unless org-noter-notes-search-path
    (setq org-noter-notes-search-path org-directory)))
