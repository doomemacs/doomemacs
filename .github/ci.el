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
(add-to-list 'doom-cli-commit-scopes '(docs "install" ci-check-docs-scope))
(add-to-list 'doom-cli-commit-scopes #'ci-check-module-scope)
;; DEPRECATED Will be removed once modules live in their own repo
(add-to-list 'doom-cli-commit-scopes '(release "modules"))


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
