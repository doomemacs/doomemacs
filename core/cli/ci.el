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


(defun doom-cli--ci-deploy-hooks ()
  (let ((dir (doom-path doom-emacs-dir ".git/hooks"))
        (default-directory doom-emacs-dir))
    (make-directory dir 'parents)
    (dolist (hook '("commit-msg" "pre-push"))
      (let ((file (doom-path dir hook)))
        (with-temp-file file
          (insert "#!/usr/bin/env sh\n"
                  (doom-path doom-emacs-dir "bin/doom")
                  " --nocolor ci hook-" hook
                  " \"$@\""))
        (set-file-modes file #o700)
        (print! (success "Created %s") (relpath file))))))


;;
;;; Git hooks

(defvar doom-cli-commit-ref-types '("Fix" "Ref" "Close" "Revert"))

(defvar doom-cli-commit-ref-git-types '("Co-authored-by:" "Signed-off-by:"))

(defvar doom-cli-commit-rules
  (list (fn! (&key subject)
          (when (<= (length subject) 10)
            (cons 'error "Subject is too short (<10) and should be more descriptive")))

        (fn! (&key subject type)
          (unless (memq type '(bump revert))
            (let ((len (length subject)))
              (cond ((> len 50)
                     (cons 'warning
                           (format "Subject is %d characters; <=50 is ideal, 72 is max"
                                   len)))
                    ((> len 72)
                     (cons 'error
                           (format "Subject is %d characters; <=50 is ideal, 72 is max"
                                   len)))))))

        (fn! (&key type)
          (unless (memq type '(bump dev docs feat fix merge module nit perf
                                    refactor release revert test tweak))
            (cons 'error (format "Commit has an invalid type (%s)" type))))

        (fn! (&key summary)
          (when (or (not (stringp summary))
                    (string-blank-p summary))
            (cons 'error "Commit has no summary")))

        (fn! (&key type summary subject)
          (and (not (eq type 'revert))
               (stringp summary)
               (string-match-p "^[A-Z][^-]" summary)
               (not (string-match-p "\\(SPC\\|TAB\\|ESC\\|LFD\\|DEL\\|RET\\)" summary))
               (cons 'error (format "%S in summary is capitalized; do not capitalize the summary"
                                    (car (split-string summary " "))))))

        (fn! (&key type scopes summary)
          (and (memq type '(bump revert release merge module))
               scopes
               (cons 'error
                     (format "Scopes for %s commits should go after the colon, not before"
                             type))))

        (fn! (&key type scopes)
          (unless (memq type '(bump revert merge module release))
            (cl-loop with valid-scopes =
                     (let ((modules (mapcar #'doom-module-from-path (cdr (doom-module-load-path (list doom-modules-dir))))))
                       (append (seq-uniq (mapcar #'car modules))
                               (mapcar #'cdr modules)))
                     with extra-scopes  = '("cli" "ci" "lib")
                     with regexp-scopes = '("^&")
                     with type-scopes =
                     (pcase type
                       (`docs
                        (cons "install"
                              (mapcar #'file-name-base
                                      (doom-glob doom-docs-dir "[a-z]*.org")))))
                     with scopes-re =
                     (concat (string-join regexp-scopes "\\|")
                             "\\|^\\("
                             (regexp-opt (append type-scopes
                                                 extra-scopes
                                                 (mapcar #'symbol-name valid-scopes)))
                             "\\)$")
                     for scope in scopes
                     if (not (string-match scopes-re scope))
                     collect scope into error-scopes
                     finally return
                     (when error-scopes
                       (cons 'error
                             (format "Commit has invalid scope%s: %s"
                                     (if (cdr error-scopes) "s" "")
                                     (string-join (nreverse error-scopes) ", ")))))))

        (fn! (&key scopes)
          (unless (equal scopes (sort scopes #'string-lessp))
            (cons 'error "Scopes are not in lexicographical order")))

        (fn! (&key type body)
          (unless (memq type '(bump revert merge))
            (catch 'result
              (with-temp-buffer
                (save-excursion (insert body))
                (while (re-search-forward "^[^\n]\\{73,\\}" nil t)
                  ;; Exclude ref lines, bump lines, comments, lines with URLs,
                  ;; or indented lines
                  (save-excursion
                    (or (let ((bump-re "\\(https?://.+\\|[^/]+\\)/[^/]+@[a-z0-9]\\{12\\}"))
                          (re-search-backward (format "^%s -> %s$" bump-re bump-re) nil t))
                        (re-search-backward "https?://[^ ]+\\{73,\\}" nil t)
                        (re-search-backward "^\\(?:#\\| +\\)" nil t)
                        (throw 'result (cons 'error "Line(s) in commit body exceed 72 characters")))))))))

        (fn! (&key bang body type)
          (if bang
              (cond ((not (string-match-p "^BREAKING CHANGE:" body))
                     (cons 'error "'!' present in commit type, but missing 'BREAKING CHANGE:' in body"))
                    ((not (string-match-p "^BREAKING CHANGE: .+" body))
                     (cons 'error "'BREAKING CHANGE:' present in commit body, but missing explanation")))
            (when (string-match-p "^BREAKING CHANGE:" body)
              (cons 'error (format "'BREAKING CHANGE:' present in body, but missing '!' after %S"
                                   type)))))

        (fn! (&key type body)
          (and (eq type 'bump)
               (let ((bump-re "\\(?:https?://.+\\|[^/]+\\)/[^/]+@\\([a-z0-9]+\\)"))
                 (not (string-match-p (concat "^" bump-re " -> " bump-re "$")
                                      body)))
               (cons 'error "Bump commit is missing commit hash diffs")))

        (fn! (&key body)
          (with-temp-buffer
            (insert body)
            (let ((bump-re "\\<\\(?:https?://[^@]+\\|[^/]+\\)/[^/]+@\\([a-z0-9]+\\)")
                  refs)
              (while (re-search-backward bump-re nil t)
                (when (/= (length (match-string 1)) 12)
                  (push (match-string 0) refs)))
              (when refs
                (cons 'error (format "%d commit hash(s) not 12 characters long: %s"
                                     (length refs) (string-join (nreverse refs) ", ")))))))

        ;; TODO Add bump validations for revert: type.

        (fn! (&key body)
          (when (string-match-p "^\\(\\(Fix\\|Clos\\|Revert\\)ed\\|Reference[sd]\\|Refs\\):? " body)
            (cons 'error "No present tense or imperative mood for a reference line")))

        (fn! (&key refs)
          (and (seq-filter (lambda (ref)
                             (string-match-p "^\\(\\(Fix\\|Close\\|Revert\\)\\|Ref\\): " ref))
                           refs)
               (cons 'error "Colon after reference line keyword; omit the colon on Fix, Close, Revert, and Ref lines")))

        (fn! (&key refs)
          (catch 'found
            (dolist (line refs)
              (cl-destructuring-bind (type . ref) (split-string line " +")
                (unless (member type doom-cli-commit-ref-git-types)
                  (setq ref (string-join ref " "))
                  (or (string-match "^\\(https?://.+\\|[^/]+/[^/]+\\)?\\(#[0-9]+\\|@[a-z0-9]+\\)" ref)
                      (string-match "^https?://" ref)
                      (and (string-match "^[a-z0-9]\\{12\\}$" ref)
                           (= (car (doom-call-process "git" "show" ref))
                              0))
                      (throw 'found
                             (cons 'error
                                   (format "%S is not a valid issue/PR, URL, or 12-char commit hash"
                                           line)))))))))

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
    (String) Contains the whole BODY of a commit message. This includes the
    subject line (the first line) and the footer.
  :refs
    (List<String>) Contains a list of reference lines, i.e. All Fix, Ref, Close,
    or Revert lines with a valid reference (an URL, commit hash, or valid Github
    issue/PR reference).
  :scopes
    (List<Symbol>) Contains a list of scopes, as symbols. e.g. with
    'feat(org,lsp): so on and so forth', this contains '(org lsp).
  :subject
    (String) Contains the whole first line of a commit message.
  :summary
    (String) Contains the summary following the type and scopes. e.g. In
    'feat(org): fix X, Y, and Z' the summary is 'fix X, Y, and Z.
  :type
    (Symbol) The type of commit this is. E.g. `feat', `fix', `bump', etc.

Each function should return nothing if there was no error, otherwise return a
cons cell whose CAR is the type of incident as a symbol (one of `error' or
`warn') and whose CDR is an explanation (string) for the result.

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
      (let ((z40 "0000000000000000000000000000000000000000")
            line range errors)
        (while (setq line (ignore-errors (read-from-minibuffer "")))
          (catch 'continue
            (cl-destructuring-bind (local-ref local-sha remote-ref remote-sha)
                (split-string line " ")
              (unless (or (string-match-p "^refs/heads/\\(master\\|main\\)$" remote-ref)
                          (equal local-sha z40))
                (throw 'continue t))
              (setq
               range (if (equal remote-sha z40)
                         local-sha
                       (format "%s..%s" remote-sha local-sha)))

              (dolist (type '("WIP" "squash!" "fixup!" "FIXUP"))
                (let ((commits
                       (split-string
                        (cdr (doom-call-process
                              "git" "rev-list" "--grep" (concat "^" type) range))
                        "\n" t)))
                  (dolist (commit commits)
                    (push (cons type commit) errors))))

              (if (null errors)
                  (print! (success "No errors during push"))
                (print! (error "Aborting push due to lingering WIP, squash!, or fixup! commits"))
                (print-group!
                 (dolist (error errors)
                   (print! (info "%s commit in %s" (car error) (cdr error)))))
                (throw 'exit 1))))))))


;;
;;;

(defun doom-cli--ci--lint (commits)
  (let ((errors? 0)
        (warnings? 0))
    (print! (start "Linting %d commits" (length commits)))
    (print-group!
     (dolist (commit commits)
       (let (subject body refs summary type scopes bang refs errors warnings)
         (with-temp-buffer
           (save-excursion (insert (cdr commit)))
           (let ((end (save-excursion
                        (or (and (re-search-forward
                                  (format "\n\n%s "
                                          (regexp-opt (append doom-cli-commit-ref-types
                                                              doom-cli-commit-ref-git-types)
                                                      t))
                                  nil t)
                                 (match-beginning 1))
                            (point-max)))))
             (setq subject (buffer-substring (point-min) (line-end-position))
                   body (buffer-substring (line-beginning-position 3) end)
                   refs (split-string (buffer-substring end (point-max))
                                      "\n" t)))
           (save-match-data
             (when (looking-at "^\\([a-zA-Z0-9_-]+\\)\\(!?\\)\\(?:(\\([^)]+\\))\\)?: \\([^\n]+\\)")
               (setq type (intern (match-string 1))
                     bang (equal (match-string 2) "!")
                     summary (match-string 4)
                     scopes (ignore-errors (split-string (match-string 3) ","))))))
         (unless (string-match-p "^\\(?:\\(?:fixup\\|squash\\)!\\|FIXUP\\|WIP\\) " subject)
           (dolist (fn doom-cli-commit-rules)
             (pcase (funcall fn
                             :bang bang
                             :body body
                             :refs refs
                             :scopes scopes
                             :subject subject
                             :summary summary
                             :type type)
               (`(,type . ,msg)
                (push msg (if (eq type 'error) errors warnings)))))
           (if (and (null errors) (null warnings))
               (print! (success "%s %s") (substring (car commit) 0 7) subject)
             (print! (start "%s %s") (substring (car commit) 0 7) subject))
           (print-group!
            (when errors
              (cl-incf errors?)
              (dolist (e (reverse errors))
                (print! (error "%s" e))))
            (when warnings
              (cl-incf warnings?)
              (dolist (e (reverse warnings))
                (print! (warn "%s" e)))))))))
    (when (> warnings? 0)
      (print! (warn "Warnings: %d") warnings?))
    (when (> errors? 0)
      (print! (error "Failures: %d") errors?))
    (if (not (or (> errors? 0) (> warnings? 0)))
        (print! (success "There were no issues!"))
      (terpri)
      (print! "See https://docs.doomemacs.org/latest/#/developers/conventions/git-commits for details")
      (when (> errors? 0)
        (throw 'exit 1)))))

(defun doom-cli--ci--read-commits ()
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
    commits))

(defun doom-cli--ci-lint-commits (from &optional to)
  (with-temp-buffer
    (insert
     (cdr (doom-call-process
           "git" "log"
           (format "%s..%s" from (or to "HEAD")))))
    (doom-cli--ci--lint (doom-cli--ci--read-commits))))
