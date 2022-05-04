;;; ci.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

;;; Types
(add-to-list 'doom-cli-commit-types 'module)
(add-to-list 'doom-cli-commit-scopeless-types 'module)


;;; Scopes
(add-to-list 'doom-cli-commit-scopes "cli")
(add-to-list 'doom-cli-commit-scopes "lib")
(add-to-list 'doom-cli-commit-scopes "docs")
(add-to-list 'doom-cli-commit-scopes #'ci-module-scope)
(add-to-list 'doom-cli-commit-scopes #'ci-docs-scope)


;;; Helpers
(cl-defun ci-module-scope (scope (&key type))
  "Only allow :CATEGORY or MODULE scopes if they actually exist."
  (seq-find (doom-rpartial
             #'doom-glob (if (string-prefix-p ":" scope)
                             (format "%s" (substring scope 1))
                           (format "*/%s" scope)))
            doom-modules-dirs))

(cl-defun ci-docs-scope (scope (&key type))
  "Allow any filename in docs/* as a scope for docs commits."
  (when (eq type 'docs)
    (member scope
            (cons "install"
                  (mapcar #'file-name-base
                          (doom-glob doom-docs-dir "*.org"))))))

;;; ci.el ends here
