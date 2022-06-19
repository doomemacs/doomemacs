;;; ci.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

(after! core-cli-ci
  ;;; Commit linter types
  (add-to-list 'doom-ci-commit-types 'module)
  (add-to-list 'doom-ci-commit-scopeless-types 'module)

  ;;; Commit linter scopes
  (add-to-list 'doom-ci-commit-scopes "cli")
  (add-to-list 'doom-ci-commit-scopes "lib")
  (add-to-list 'doom-ci-commit-scopes "docs")
  (add-to-list 'doom-ci-commit-scopes '(docs "install" ci-check-docs-scope))
  (add-to-list 'doom-ci-commit-scopes #'ci-check-module-scope)
  ;; DEPRECATED Will be removed once modules live in their own repo
  (add-to-list 'doom-ci-commit-scopes '(release "modules")))

(after! core-cli-make
  ;;; Codeowners
  ;; (dolist (path (cdr (doom-module-load-path (list doom-modules-dir))))
  ;;   (save-match-data
  ;;     (when (string-match "/modules/\\([^/]+\\)/\\([^/]+\\)/$" path)
  ;;       (add-to-list 'doom-make-codeowners
  ;;                    (cons (format "%s*" (substring (match-string 0 path) 1))
  ;;                          (list "@doomemacs/maintainers"
  ;;                                (format "@doomemacs/%s-%s"
  ;;                                        (match-string 1 path)
  ;;                                        (match-string 2 path))))))))

  ;;; Documentation exporters
  ;; (add-to-list 'doom-ci-docs-title-replace '("^Doom Emacs " . ""))
  )


;;; Helpers
(cl-defun ci-check-module-scope (scope (&key type &allow-other-keys))
  "Only allow :CATEGORY or MODULE scopes if they actually exist."
  (seq-find (doom-rpartial
             #'doom-glob (if (string-prefix-p ":" scope)
                             (format "%s" (substring scope 1))
                           (format "*/%s" scope)))
            (list (doom-dir (dir!) "../modules/")
                  (doom-dir doom-private-dir "modules/"))))

(defun ci-check-docs-scope (scope _)
  "Allow any filename in docs/* as a scope for docs commits."
  (member
   scope (doom-files-in (doom-path (dir!) "../docs")
                        :match "\\.org$"
                        :map #'file-name-base)))

;;; ci.el ends here
