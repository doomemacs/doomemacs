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
    (dolist (hook '("commit-msg"))
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

(defvar doom-cli-commit-rules
  (list (cons "^[^\n]\\{10,\\}\n"
              "Commit summary is too short (<10) and should be more descriptive")

        (cons "^\\(revert!?:[^\n]\\{,72\\}\\|[^\n]\\{,80\\}\\)\n"
              "Commit summary too long; <=72 is ideal, 80 is max")

        (cons (concat
               "^\\("
               (regexp-opt
                '("bump" "dev" "docs" "feat" "fix" "merge" "module" "nit" "perf"
                  "refactor" "release" "revert" "test" "tweak"))
               "\\)!?[^ :]*: ")
              "Invalid type")

        (cons (lambda ()
                (looking-at "^\\(bump\\|revert\\|release\\|merge\\|module\\)!?([^)]+):"))
              "This commit type's scope goes after the colon, not before")

        (cons (lambda ()
                (when (looking-at "[^ :!(]+!?(\\([^)]+\\)): ")
                  (not
                   (string-match
                    (string-join
                     (cons (concat
                            "^" (regexp-opt
                                 (cl-loop for path
                                          in (cdr (doom-module-load-path (list doom-modules-dir)))
                                          for (_category . module)
                                          = (doom-module-from-path path)
                                          collect (symbol-name module))) "$")
                           '("^&" "^cli$"))
                     "\\|")
                    (match-string 1)))))
              "Invalid scope")

        (cons (lambda ()
                (when (looking-at "[^ :!(]+![(:]")
                  (not (re-search-forward "^BREAKING CHANGE: .+" nil t))))
              "'!' present in type, but missing 'BREAKING CHANGE:' in body")

        (cons (lambda ()
                (when (looking-at "[^ :!]+\\(([^)])\\)?: ")
                  (re-search-forward "^BREAKING CHANGE: .+" nil t)))
              "'BREAKING CHANGE:' present in body, but missing '!' in type")

        (cons (lambda ()
                (when (re-search-forward "^BREAKING CHANGE:" nil t)
                  (not (looking-at " [^\n]+"))))
              "'BREAKING CHANGE:' present in body, but empty")

        (cons (lambda ()
                (when (looking-at "bump: ")
                  (let ((bump-re "^\\(https?://.+\\|[^/]+\\)/[^/]+@[a-z0-9]\\{7\\}"))
                    (re-search-forward (concat "^" bump-re " -> " bump-re "$")
                                       nil t))))
              "Bump commit doesn't contain commit diff")

        ;; TODO Add bump validations for revert: type.

        (cons (lambda ()
                (re-search-forward "^\\(\\(Fix\\|Clos\\|Revert\\)ed\\|Reference[sd]\\): "
                                   nil t))
              "Use present tense/imperative voice for references, without a colon")

        ;; TODO Check that bump/revert SUMMARY list: 1) valid modules and 2)
        ;;      modules whose files are actually being touched.

        ;; TODO Ensure your diff corraborates your SCOPE
        ))

(defun doom-cli--ci-hook-commit-msg (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let (errors)
      (dolist (rule doom-cli-commit-rules)
        (cl-destructuring-bind (pred . msg) rule
          (goto-char (point-min))
          (save-match-data
            (when (if (functionp pred)
                      (funcall pred)
                    (if (stringp pred)
                        (not (re-search-forward pred nil t))
                      (error "Invalid predicate: %S" pred)))
              (push msg errors)))))
      (when errors
        (print! (error "Your commit message failed to pass Doom's conventions, here's why:"))
        (dolist (error (reverse errors))
          (print-group! (print! (info error))))
        (terpri)
        (print! "See https://gist.github.com/hlissner/4d78e396acb897d9b2d8be07a103a854 for details")
        (throw 'exit 0))
      t)))


;;
;;;

(defun doom-cli--ci-lint-commits (from &optional to)
  (with-temp-buffer
    (save-excursion
      (insert
       (cdr (doom-call-process
             "git" "log"
             (format "%s...%s" from (or to "HEAD"))))))
    (while (re-search-forward "^commit \\([a-z0-9]\\{40\\}\\)" nil t)
      (let ((commit (match-string 1))
            errors)
        (forward-line 4)
        (save-restriction
          (save-match-data
            (narrow-to-region
             (point) (save-excursion
                       (if (re-search-forward "^commit \\([a-z0-9]\\{40\\}\\)" nil t)
                           (match-beginning 0)
                         (point-max))))
            (indent-rigidly (point-min) (point-max) -4))
          (save-excursion
            (print! (start "Commit %s") commit)
            (dolist (rule doom-cli-commit-rules)
              (cl-destructuring-bind (pred . msg) rule
                (goto-char (point-min))
                (save-match-data
                  (when (if (functionp pred)
                            (funcall pred)
                          (if (stringp pred)
                              (not (re-search-forward pred nil t))
                            (error "Invalid predicate: %S" pred)))
                    (push msg (alist-get commit errors nil nil #'equal)))))))
          (when errors
            (dolist (error (reverse errors))
              (print! (error "Commit %s") (car error))
              (print-group!
               (dolist (e (reverse (cdr error)))
                 (print! (info e)))))
            (terpri)
            (print! "%d commit(s) failed the linter" (length errors))
            (terpri)
            (print! "See https://doomemacs.org/project.org#commit-message-formatting for details")
            (throw 'exit 1)))))
    t))
