;;; core/cli/ci.el -*- lexical-binding: t; -*-

(defcli! ci (&optional target &rest args)
  "TODO"
  (unless target
    (user-error "No CI target given"))
  (if-let (fn (intern-soft (format "doom-cli--ci-%s" target)))
      (apply fn args)
    (user-error "No known CI target: %S" target)))


;;
;;;


(defun doom-cli--ci-deploy-hooks (&optional noforce)
  (let* ((default-directory doom-emacs-dir)
         (dir (cdr (doom-call-process "git" "rev-parse" "--git-path" "hooks"))))
    (make-directory dir 'parents)
    (dolist (hook '("commit-msg" "pre-push"))
      (let ((file (doom-path dir hook)))
        (unless (and (file-exists-p file) noforce)
          (with-temp-file file
            (insert "#!/usr/bin/env sh\n"
                    (doom-path doom-emacs-dir "bin/doom")
                    " --nocolor ci hook-" hook
                    " \"$@\""))
          (set-file-modes file #o700)
          (print! (success "Created %s") (relpath file)))))))


;;
;;; Git hooks

(defvar doom-cli-commit-trailer-keys
  '(("Fix" ref hash url)
    ("Ref" ref hash url)
    ("Close" ref)
    ("Revert" ref hash)
    ("Amend" ref hash)
    ("Co-authored-by" name)
    ("Signed-off-by" name))
  "An alist of valid trailer keys and their accepted value types.

Accapted value types can be one or more of ref, hash, url, username, or name.")

(defvar doom-cli-commit-trailer-types
  '((ref      . "^\\(https?://[^ ]+\\|[^/]+/[^/]+\\)?#[0-9]+$")
    (hash     . "^\\(https?://[^ ]+\\|[^/]+/[^/]+@\\)?[a-z0-9]\\{12\\}$")
    (url      . "^https?://")
    (name     . "^[a-zA-Z0-9-_ \\.']+<[^@]+@[^.]+\\.[^>]+>$")
    (username . "^@[^a-zA-Z0-9_-]+$"))
  "An alist of valid trailer keys and their accepted value types.

Accapted value types can be one or more of ref, hash, url, username, or name.")

(defvar doom-cli-commit-types
  '(bump dev docs feat fix merge module nit perf refactor release revert test tweak)
  "A list of valid commit types.")

(defvar doom-cli-commit-scopes
  (list "cli"
        "ci"
        "lib"
        "docs"
        (fn! (scope (&key type))
          (when (and (memq type '(bump merge module release revert))
                     scope)
            (user-error "%s commits should never have a scope" type)))
        (fn! (scope _)
          (seq-find (doom-rpartial
                     #'doom-glob (if (string-prefix-p ":" scope)
                                     (format "%s" (substring scope 1))
                                   (format "*/%s" scope)))
                    doom-modules-dirs)))
  "A list of valid commit scopes as strings or functions.

Functions should take two arguments: a single scope (symbol) and a commit plist
representing the current commit being checked against. See
`doom-cli-commit-core-rules' for possible values.")

(defvar doom-cli-commit-rules
  (list (fn! (&key subject)
          "If a fixup/squash commit, don't lint this commit"
          (when (string-match "^\\(\\(?:fixup\\|squash\\)!\\|FIXUP\\|WIP\\) " subject)
            (skip! (format "Found %S commit, skipping commit" (match-string 1 subject)))))

        (fn! (&key type subject)
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

        (fn! (&key type)
          "Ensure commit has valid type"
          (or (memq type doom-cli-commit-types)
              (if type
                  (fail! "Invalid commit type: %s" type)
                (fail! "Commit has no detectable type"))))

        (fn! (&key summary)
          "Ensure commit has a summary"
          (when (or (not (stringp summary))
                    (string-blank-p summary))
            (fail! "Commit has no summary")))

        (fn! (&key type summary subject)
          "Ensure summary isn't needlessly capitalized"
          (and (stringp summary)
               (string-match-p "^[A-Z][^-A-Z.]" summary)
               (fail! "%S in summary should not be capitalized"
                      (car (split-string summary " ")))))

        (fn! (&key type scopes summary)
          "Complain about scoped types that are incompatible with scopes"
          (and (memq type '(bump revert merge module release))
               scopes
               (fail! "Scopes for %s commits should go after the colon, not before"
                      type)))

        (fn! (&rest plist &key type scopes)
          "Ensure scopes are valid"
          (dolist (scope scopes)
            (condition-case e
                (or (cl-loop for rule in doom-cli-commit-scopes
                             if (or (and (stringp rule)
                                         (string= rule scope))
                                    (and (functionp rule)
                                         (funcall rule scope plist)))
                             return t)
                    (fail! "Invalid scope: %s" scope))
              (user-error (fail! "%s" (error-message-string))))))

        (fn! (&key scopes)
          "Esnure scopes are sorted correctly"
          (unless (equal scopes (sort (copy-sequence scopes) #'string-lessp))
            (fail! "Scopes are not in lexicographical order")))

        (fn! (&key type body)
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

        (fn! (&key bang body type)
          "Ensure ! is accompanied by a 'BREAKING CHANGE:' in BODY"
          (if bang
              (cond ((not (string-match-p "^BREAKING CHANGE:" body))
                     (fail! "'!' present in commit type, but missing 'BREAKING CHANGE:' in body"))
                    ((not (string-match-p "^BREAKING CHANGE: .+" body))
                     (fail! "'BREAKING CHANGE:' present in commit body, but missing explanation")))
            (when (string-match-p "^BREAKING CHANGE:" body)
              (fail! "'BREAKING CHANGE:' present in body, but missing '!' after %S"
                     type))))

        (fn! (&key type body)
          "Ensure bump commits have package ref lines"
          (and (eq type 'bump)
               (let ((bump-re "\\(?:https?://.+\\|[^/]+\\)/[^/]+@\\([a-z0-9]+\\)"))
                 (not (string-match-p (concat "^" bump-re " -> " bump-re "$")
                                      body)))
               (fail! "Bump commit is missing commit hash diffs")))

        (fn! (&key body)
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
        (fn! (&key body trailers)
          "Validate commit trailers."
          (let* ((keys   (mapcar #'car doom-cli-commit-trailer-keys))
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
              (if (and (not (memq 'name (cdr (assoc key doom-cli-commit-trailer-keys))))
                       (string-match-p " " value))
                  (fail! "Found %S, but only one value allowed per trailer"
                         (truncate-string-to-width (concat key ": " value) 20 nil nil "…"))
                (when-let (allowed-types (cdr (assoc key doom-cli-commit-trailer-keys)))
                  (or (cl-loop for type in allowed-types
                               if (cdr (assq type doom-cli-commit-trailer-types))
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

(defun doom-cli--ci-hook-commit-msg (file)
  (with-temp-buffer
    (insert-file-contents file)
    (doom-cli--ci--lint
     (list (cons
            "CURRENT"
            (buffer-substring (point-min)
                              (if (re-search-forward "^# Please enter the commit message" nil t)
                                  (match-beginning 0)
                                (point-max))))))))

(defun doom-cli--ci-hook-pre-push (_remote _url)
  (with-temp-buffer
    (let ((z40 (make-string 40 ?0))
          line error)
      (while (setq line (ignore-errors (read-from-minibuffer "")))
        (catch 'continue
          (seq-let (local-ref local-sha remote-ref remote-sha)
              (split-string line " ")
            (unless (or (string-match-p "^refs/heads/\\(master\\|main\\)$" remote-ref)
                        (equal local-sha z40))
              (throw 'continue t))
            (print-group!
             (mapc (lambda (commit)
                     (seq-let (hash msg) (split-string commit "\t")
                       (setq error t)
                       (print! (info "%S commit in %s"
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
               (throw 'exit 1)))))))))


;;
;;;

(defun doom-cli--parse-commit (commit-msg)
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

(defun doom-cli--parse-bumps (from end)
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

(defun doom-cli--ci--lint (commits)
  (let ((warnings 0)
        (failures 0))
    (print! (start "Linting %d commits" (length commits)))
    (print-group!
     (pcase-dolist (`(,ref . ,commitmsg) commits)
       (let* ((commit   (doom-cli--parse-commit commitmsg))
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
                    doom-cli-commit-rules)))))))
    (let ((issues (+ warnings failures)))
      (if (= issues 0)
          (print! (success "There were no issues!"))
        (if (> warnings 0) (print! (warn "Warnings: %d" warnings)))
        (if (> failures 0) (print! (warn "Failures: %d" failures)))
        (print! "\nSee https://docs.doomemacs.org/-/conventions/git-commits")
        (unless (zerop failures)
          (throw 'exit 1)))
      t)))

(defun doom-cli--ci-lint-commits (from &optional to)
  (with-temp-buffer
    (insert
     (cdr (doom-call-process
           "git" "log"
           (format "%s...%s" from (or to (concat from "~1"))))))
    (doom-cli--ci--lint
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
