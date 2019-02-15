;; -*- no-byte-compile: t; -*-
;;; tools/magit/packages.el

;; Magit introduced some breaking changes in a recent commit that breaks
;; evil-magit and magithub.
;; TODO Once those packages have updated, we'll go back to tracking magit HEAD
(when (package! magit :recipe
        (:fetcher github
         :repo "magit/magit"
         :commit "78114e6425d5e7d6eaa7712c845f28694aa7faeb"
         :files ("lisp/magit" "lisp/magit*.el" "lisp/git-rebase.el")))
  (package! magit-gitflow)
  (if (featurep! +forge)
      (package! forge)
    (package! magithub))
  (package! magit-todos)
  (when (featurep! :feature evil +everywhere)
    (package! evil-magit)))
