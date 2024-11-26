;;; lisp/cli/ci.el -*- lexical-binding: t; -*-

;;
;;; Variables

(defvar doom-ci-commit-trailer-keys
  '(("Fix" ref hash url)
    ("Ref" ref hash url)
    ("Close" ref)
    ("Revert" ref hash)
    ("Amend" ref hash)
    ("Co-authored-by" name)
    ("Signed-off-by" name))
  "An alist of valid trailer keys and their accepted value types.

Accapted value types can be one or more of ref, hash, url, username, or name.")

(defvar doom-ci-commit-trailer-types
  '((ref      . "^\\(https?://[^ ]+\\|[^/]+/[^/]+\\)?#[0-9]+$")
    (hash     . "^\\(https?://[^ ]+\\|[^/]+/[^/]+@\\)?[a-z0-9]\\{12\\}$")
    (url      . "^https?://")
    (name     . "^[a-zA-Z0-9-_ \\.']+<[^@]+@[^.]+\\.[^>]+>$")
    (username . "^@[^a-zA-Z0-9_-]+$"))
  "An alist of valid trailer keys and their accepted value types.

Accapted value types can be one or more of ref, hash, url, username, or name.")

(defvar doom-ci-commit-types
  '(bump dev docs feat fix merge nit perf refactor release revert test tweak)
  "A list of valid commit types.")

(defvar doom-ci-commit-scopeless-types '(bump merge release revert)
  "A list of commit types whose scopes should be passed in its BODY.

  Don't: \"bump(SCOPE): ...\"
  Do:    \"bump: SCOPE\"")

(defvar doom-ci-commit-scopes '("ci" doom-ci-enforce-scopeless-types)
  "A list of valid commit scopes as strings, predicate functions, or lists.

These are checked against each item in the comma-delimited scope field of the
current commit's message. E.g. 'fix(foo,bar,baz): ...' => foo, bar and baz

Each element of this list can be one of:

- A string, compared literally against the scope's name.
- A function predicate, taking two arguments (a scope as a symbol, and a plist
  containing information about the current commit--see `doom-ci-commit-scopes'
  for more about its structure). These predicates should:
  - Return non-nil to immediately pass a scope.
  - Throw a `user-error' to immediately fail the scope.
  - Return nil to continue with the checks in this list.
- A list, denoting type-specific scopes. Its CAR is the type as a symbol, and
  its CDR is a nested list of scopes as strings/predicates. E.g.

    '(docs \"faq\" \"install\" check-docs)")

(defvar doom-ci-commit-rules
  ;; TODO Extract into named functions
  (list (lambda! (&key subject)
          "If a fixup/squash commit, don't lint this commit"
          (when (string-match "^\\(\\(?:fixup\\|squash\\)!\\|FIXUP\\|WIP\\) " subject)
            (skip! (format "Found %S commit, skipping commit" (match-string 1 subject)))))

        (lambda! (&key type subject)
          "Test SUBJECT length"
          (let ((len (length subject)))
            (cond ((memq type '(bump revert)))
                  ((<= len 10)
                   (fail! "Subject is too short (<10) and should be more descriptive"))
                  ((<= len 20)
                   (warn! "Subject is short (<20); are you sure it's descriptive enough?"))
                  ((> len 72)
                   (fail! "Subject is %d characters, above the 72 maximum"
                          len))
                  ((> len 50)
                   (warn! "Subject is %d characters; <=50 is ideal"
                          len)))))

        (lambda! (&key type)
          "Ensure commit has valid type"
          (or (memq type doom-ci-commit-types)
              (if type
                  (fail! "Invalid commit type: %s" type)
                (fail! "Commit has no detectable type"))))

        (lambda! (&key summary)
          "Ensure commit has a summary"
          (when (or (not (stringp summary))
                    (string-blank-p summary))
            (fail! "Commit has no summary")))

        (lambda! (&key type summary subject)
          "Ensure summary isn't needlessly capitalized"
          (and (stringp summary)
               (string-match-p "^[A-Z][^-A-Z.]" summary)
               (fail! "%S in summary should not be capitalized"
                      (car (split-string summary " ")))))

        (lambda! (&rest plist &key type scopes)
          "Ensure scopes are valid"
          (dolist (scope scopes)
            (condition-case e
                (letf! (defun* check-rule (rule)
                         (or (and (stringp rule)
                                  (string= rule scope))
                             (and (functionp rule)
                                  (funcall rule scope plist))
                             (and (listp rule)
                                  (eq type (car rule))
                                  (seq-find #'check-rule (cdr rule)))))
                  (or (seq-find #'check-rule doom-ci-commit-scopes)
                      (fail! "Invalid scope: %s" scope)))
              (user-error (fail! "%s" (error-message-string e))))))

        (lambda! (&key scopes)
          "Esnure scopes are sorted correctly"
          (unless (equal scopes (sort (copy-sequence scopes) #'string-lessp))
            (fail! "Scopes are not in lexicographical order")))

        (lambda! (&key type body)
          "Enforce 72 character line width for BODY"
          (catch 'result
            (with-temp-buffer
              (save-excursion (insert body))
              (while (re-search-forward "^[^\n]\\{73,\\}" nil t)
                (save-excursion
                  (or
                   ;; Long bump lines are acceptable
                   (let ((bump-re "\\(https?://.+\\|[^/]+\\)/[^/]+@[a-z0-9]\\{12\\}"))
                     (re-search-backward (format "^%s -> %s$" bump-re bump-re) nil t))
                   ;; Long URLs are acceptable
                   (re-search-backward "https?://[^ ]+\\{73,\\}" nil t)
                   ;; Lines that start with # or whitespace are comment or
                   ;; code blocks.
                   (re-search-backward "^\\(?:#\\| +\\)" nil t)
                   (throw 'result (fail! "Line(s) in commit body exceed 72 characters"))))))))

        (lambda! (&key bang body type)
          "Ensure ! is accompanied by a 'BREAKING CHANGE:' in BODY"
          (if bang
              (cond ((not (string-match-p "^BREAKING CHANGE:" body))
                     (fail! "'!' present in commit type, but missing 'BREAKING CHANGE:' in body"))
                    ((not (string-match-p "^BREAKING CHANGE: .+" body))
                     (fail! "'BREAKING CHANGE:' present in commit body, but missing explanation")))
            (when (string-match-p "^BREAKING CHANGE:" body)
              (fail! "'BREAKING CHANGE:' present in body, but missing '!' after %S"
                     type))))

        (lambda! (&key type body)
          "Ensure bump commits have package ref lines"
          (and (eq type 'bump)
               (let ((bump-re "\\(?:https?://.+\\|[^/]+\\)/[^/]+@\\([a-z0-9]+\\)"))
                 (not (string-match-p (concat "^" bump-re " -> " bump-re "$")
                                      body)))
               (fail! "Bump commit is missing commit hash diffs")))

        (lambda! (&key body)
          "Ensure commit hashes in bump lines are 12 characters long"
          (with-temp-buffer
            (insert body)
            (let ((bump-re "\\<\\(?:https?://[^@]+\\|[^/]+\\)/[^/]+@\\([a-z0-9]+\\)")
                  refs)
              (while (re-search-backward bump-re nil t)
                (when (/= (length (match-string 1)) 12)
                  (push (match-string 0) refs)))
              (when refs
                (fail! "%d commit hash(s) not 12 characters long: %s"
                       (length refs) (string-join (nreverse refs) ", "))))))

        ;; TODO Add bump validations for revert: type.
        (lambda! (&key body trailers)
          "Validate commit trailers."
          (let* ((keys   (mapcar #'car doom-ci-commit-trailer-keys))
                 (key-re (regexp-opt keys t))
                 (lines
                  ;; Scan BODY because invalid trailers won't be in TRAILERS.
                  (save-match-data
                    (and (string-match "\n\\(\n[a-zA-Z][a-zA-Z-]*:? [^ ][^\n]+\\)+\n+\\'" body)
                         (split-string (match-string 0 body) "\n" t)))))
            (dolist (line lines)
              (unless (string-match-p (concat "^" key-re ":? [^ ]") line)
                (fail! "Found %S, expected one of: %s"
                       (truncate-string-to-width (string-trim line) 16 nil nil "…")
                       (string-join keys ", ")))
              (when (and (string-match "^[^a-zA-Z-]+:? \\(.+\\)$" line)
                         (string-match-p " " (match-string 1 line)))
                (fail! "%S has multiple references, but should only have one per line"
                       (truncate-string-to-width (string-trim line) 20 nil nil "…")))
              (when (or (string-match (concat "^" key-re "\\(?:e?[sd]\\|ing\\)? [^ ]") line)
                        (string-match (concat "^\\([a-zA-Z-]+\\) [^ \n]+$") line))
                (fail! "%S missing colon after %S"
                       (truncate-string-to-width (string-trim line) 16 nil nil "…")
                       (match-string 1 line))))
            (pcase-dolist (`(,key . ,value) trailers)
              (if (and (not (memq 'name (cdr (assoc key doom-ci-commit-trailer-keys))))
                       (string-match-p " " value))
                  (fail! "Found %S, but only one value allowed per trailer"
                         (truncate-string-to-width (concat key ": " value) 20 nil nil "…"))
                (when-let (allowed-types (cdr (assoc key doom-ci-commit-trailer-keys)))
                  (or (cl-loop for type in allowed-types
                               if (cdr (assq type doom-ci-commit-trailer-types))
                               if (string-match-p it value)
                               return t)
                      (fail! "%S expects one of %s, but got %S"
                             key allowed-types value)))))))

        ;; TODO Check that bump/revert SUBJECT list: 1) valid modules and 2)
        ;;      modules whose files are actually being touched.

        ;; TODO Ensure your diff corraborates your SCOPE

        )
  "A list of validator functions to run against a commit.

Each function is N-arity and is passed a plist with the following keys:

  :bang
    (Boolean) If `t', the commit is declared to contain a breaking change.
    e.g. 'refactor!: this commit breaks everything'
  :body
    (String) Contains the whole BODY of a commit message, excluding the
    TRAILERS.
  :scopes
    (List<Symbol>) Contains a list of scopes, as symbols. e.g. with
    'feat(org,lsp): so on and so forth', this contains '(org lsp).
  :subject
    (String) Contains the whole first line of a commit message.
  :summary
    (String) Contains the summary following the type and scopes. e.g. In
    'feat(org): fix X, Y, and Z' the summary is 'fix X, Y, and Z.
  :trailers
    (Map<String, String>) Contains an alist of 'KEY: VALUE' trailers, i.e. All
    Fix, Ref, Close, Revert, etc lines with a valid value. This will be empty if
    the formatting of a commit's trailers is invalid.
  :type
    (Symbol) The type of commit this is. E.g. `feat', `fix', `bump', etc.

Each function should call `fail!' or `warn!' one or more times, or `skip!'
(immediately returns). Each of these lexical functions take the same arguments
as `format'.

Note: warnings are not considered failures.")


;;
;;; Commands

;;; doom ci
(defcli! ci (&args _)
  "Commands that automate development processes."
  :partial t)

;; TODO Move to 'doom install --git-hooks'
(defcli! (ci deploy-hooks) ((force ("--force")))
  "TODO"
  (let* ((repo-path (sh! "git" "rev-parse" "--show-toplevel"))
         (repo-path (if (zerop (car repo-path))
                        (cdr repo-path)
                      (user-error "Cannot locate a git repo in %s"
                                  (file-relative-name default-directory))))
         (submodule-p (string-empty-p (cdr (sh! "git" "rev-parse" "show-superproject-working-tree"))))
         (config-hooks-path (cdr (sh! "git" "config" "core.hooksPath")))
         (hooks-path (cdr (sh! "git" "rev-parse" "--git-path" "hooks"))))
    (unless (string-empty-p config-hooks-path)
      (or force
          (y-or-n-p
           (format (concat "Detected non-standard core.hookPath: %S\n\n"
                           "Install Doom's commit-msg and pre-push git hooks anyway?")
                   hooks-path))
          (user-error "Aborted")))
    (make-directory hooks-path 'parents)
    (dolist (hook '("commit-msg" "pre-push"))
      (let* ((hook (doom-path hooks-path hook))
             (overwrite-p (file-exists-p hook)))
        (with-temp-file hook
          (insert "#!/usr/bin/env sh\n"
                  (doom-path doom-emacs-dir "bin/doom")
                  " --no-color ci hook " (file-name-base hook)
                  " \"$@\""))
        (set-file-modes hook #o700)
        (print! (success "%s %s")
                (if overwrite-p "Overwrote" "Created")
                (path hook))))))

;; TODO Move to 'doom lint commits'
(defcli! (ci lint-commits) (from &optional to)
  "TODO"
  (with-temp-buffer
    (insert
     (cdr (doom-call-process
           "git" "log"
           (format "%s...%s" from (or to (concat from "~1"))))))
    (doom-ci--lint
     (let (commits)
       (while (re-search-backward "^commit \\([a-z0-9]\\{40\\}\\)" nil t)
         (push (cons (match-string 1)
                     (replace-regexp-in-string
                      "^    " ""
                      (save-excursion
                        (buffer-substring-no-properties
                         (search-forward "\n\n")
                         (if (re-search-forward "\ncommit \\([a-z0-9]\\{40\\}\\)" nil t)
                             (match-beginning 0)
                           (point-max))))))
               commits))
       commits))))

;; TODO Move to 'doom lint hook:commit-msg'
(defcli! (ci hook commit-msg) (file)
  "Run git commit-msg hook.

Lints the current commit message."
  (with-temp-buffer
    (insert-file-contents file)
    (doom-ci--lint
     `(("CURRENT" .
        ,(buffer-substring
          (point-min)
          (if (re-search-forward "^# Please enter the commit message" nil t)
              (match-beginning 0)
            (point-max))))))))

;; TODO Move to 'doom lint hook:pre-push'
(defcli! (ci hook pre-push) (remote url)
  "Run git pre-push hook.

Prevents pushing if there are unrebased or WIP commits."
  (with-temp-buffer
    (let ((z40 (make-string 40 ?0))
          line error)
      (while (setq line (ignore-errors (read-from-minibuffer "")))
        (catch 'continue
          (seq-let (local-ref local-sha remote-ref remote-sha)
              (split-string line " ")
            ;; TODO Extract this branch detection to a variable
            (unless (or (string-match-p "^refs/heads/\\(master\\|main\\)$" remote-ref)
                        (equal local-sha z40))
              (throw 'continue t))
            (print-group!
              (mapc (lambda (commit)
                      (seq-let (hash msg) (split-string commit "\t")
                        (setq error t)
                        (print! (item "%S commit in %s"
                                      (car (split-string msg " "))
                                      (substring hash 0 12)))))
                    (split-string
                     (cdr (doom-call-process
                           "git" "rev-list"
                           "--grep" (concat "^" (regexp-opt '("WIP" "squash!" "fixup!" "FIXUP") t) " ")
                           "--format=%H\t%s"
                           (if (equal remote-sha z40)
                               local-sha
                             (format "%s..%s" remote-sha local-sha))))
                     "\n" t))
              (when error
                (print! (error "Aborting push due to unrebased WIP, squash!, or fixup! commits"))
                (exit! 1)))))))))


;;
;;; Helpers

(cl-defun doom-ci-enforce-scopeless-types (scope (&key type scopes summary &allow-other-keys))
  "Complain about scoped commit types that shouldn't be scoped."
  (when (memq type doom-ci-commit-scopeless-types)
    (user-error "Scopes for %s commits should go after the colon, not before"
                type)))


(defun doom-ci--parse-commit (commit-msg)
  (with-temp-buffer
    (save-excursion (insert commit-msg))
    (append
     (let ((end
            (save-excursion
              (if (re-search-forward "\n\\(\n[a-zA-Z-]+: [^ ][^\n]+\\)+\n*\\'" nil t)
                  (1+ (match-beginning 0))
                (point-max)))))
       `(:subject  ,(buffer-substring (point-min) (line-end-position))
         :body     ,(string-trim-right (buffer-substring (line-beginning-position 3) end))
         :trailers ,(save-match-data
                      (cl-loop with footer = (buffer-substring end (point-max))
                               for line in (split-string footer "\n" t)
                               if (string-match "^\\([a-zA-Z-]+\\): \\(.+\\)$" line)
                               collect (cons (match-string 1 line) (match-string 2 line))))))
     (save-match-data
       (when (looking-at "^\\([a-zA-Z0-9_-]+\\)\\(!?\\)\\(?:(\\([^)]+\\))\\)?: \\([^\n]+\\)")
         `(:type    ,(intern (match-string 1))
           :bang    ,(equal (match-string 2) "!")
           :summary ,(match-string 4)
           :scopes  ,(ignore-errors (split-string (match-string 3) ",")))))
     (save-excursion
       (let ((bump-re "\\(\\(?:https?://.+\\|[^/ \n]+\\)/[^/ \n]+@[a-z0-9]\\{12\\}\\)")
             bumps)
         (while (re-search-forward (format "^\\s-*\\<%s -> %s\\>" bump-re bump-re) nil t)
           (cond ((rassoc (match-string 1) bumps)
                  (setcdr (rassoc (match-string 1) bumps) (match-string 2)))
                 ((assoc (match-string 2) bumps)
                  (setcar (assoc (match-string 2) bumps) (match-string 1)))
                 ((setf (alist-get (match-string 1) bumps nil nil #'equal)
                        (match-string 2)))))
         `(:bumps ,(cl-sort (delete-dups bumps) #'string-lessp :key #'car)))))))

(defun doom-ci--parse-bumps (from end)
  (with-temp-buffer
    (save-excursion
      (insert
       (cdr (doom-call-process "git" "log" "--format=full" "--grep=\\(bump\\|revert\\):"
                               (format "%s...%s" from end)))))
    (save-match-data
      (let (packages)
        (while (let ((bump-re "\\(\\(?:https?://.+\\|[^/ ]+\\)/[^/ ]+@[a-z0-9]\\{12\\}\\)"))
                 (re-search-forward (format "^\\s-*\\<%s -> %s\\>" bump-re bump-re) nil t))
          (cond ((rassoc (match-string 1) packages)
                 (setcdr (rassoc (match-string 1) packages) (match-string 2)))
                ((assoc (match-string 2) packages)
                 (setcar (assoc (match-string 2) packages) (match-string 1)))
                ((setf (alist-get (match-string 1) packages nil nil #'equal)
                       (match-string 2)))))
        (cl-sort (delete-dups packages) #'string-lessp :key #'car)))))

(defun doom-ci--lint (commits)
  (let ((warnings 0)
        (failures 0))
    (print! (start "Linting %d commits" (length commits)))
    (print-group!
      (pcase-dolist (`(,ref . ,commitmsg) commits)
        (let* ((commit   (doom-ci--parse-commit commitmsg))
               (shortref (substring ref 0 7))
               (subject  (plist-get commit :subject)))
          (cl-block 'linter
            (letf! ((defun skip! (reason &rest args)
                      (print! (warn "Skipped because: %s") (apply #'format reason args))
                      (cl-return-from 'linter))
                    (defun warn! (reason &rest args)
                      (cl-incf warnings)
                      (print! (warn "%s") (apply #'format reason args)))
                    (defun fail! (reason &rest args)
                      (cl-incf failures)
                      (print! (error "%s") (apply #'format reason args))))
              (print! (start "%s %s") shortref subject)
              (print-group!
                (mapc (doom-rpartial #'apply commit)
                      doom-ci-commit-rules)))))))
    (let ((issues (+ warnings failures)))
      (if (= issues 0)
          (print! (success "There were no issues!"))
        (if (> warnings 0) (print! (warn "Warnings: %d" warnings)))
        (if (> failures 0) (print! (warn "Failures: %d" failures)))
        (print! "\nSee https://discourse.doomemacs.org/git-conventions")
        (unless (zerop failures)
          (exit! 1)))
      t)))

(provide 'doom-cli-ci)
;;; ci.el ends here
