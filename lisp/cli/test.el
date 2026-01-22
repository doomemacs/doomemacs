;;; lisp/cli/test.el -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; The heart of Doom's test DSL and framework. Powered by ERT (Emacs Regression
;; Testing), this provides a CLI for running unit tests in Doom Emacs.
;;
;;; Code:

;;
;;; Variables

(defvar doom-test-dir (doom-path doom-emacs-dir "test")
  "The directory where Doom's tests are stored.")


;;
;;; Commands

(defcli! test
    ((verbose ("-v" "--verbose") "Show detailed test output")
     &rest targets)
  "Run Doom unit tests.

TARGETS can be:
  - A path to a specific test file
  - A path to a directory containing test files
  - A pattern to match test files (e.g., 'packages' matches test-packages.el)

If no targets are specified, all tests in the test/ directory are run.

Examples:
  doom test                           ; Run all tests
  doom test test/lisp/lib             ; Run tests in a directory
  doom test packages                  ; Run test-packages.el
  doom test test/lisp/lib/test-packages.el  ; Run specific file"
  :benchmark nil
  (require 'ert)
  (doom-initialize t)
  (doom-startup)
  (let* ((test-files (doom-test--find-test-files targets))
         (total-tests 0)
         (passed-tests 0)
         (failed-tests 0))
    (if (null test-files)
        (progn
          (print! (warn "No test files found"))
          (exit! 1))
      ;; Load all test files
      (print! (start "Loading %d test file(s)..." (length test-files)))
      (dolist (file test-files)
        (print! (item "%s") (abbreviate-file-name file))
        (load file nil (not verbose)))
      ;; Run the tests
      (print! (start "Running tests..."))
      (terpri)
      (let* ((ert-quiet (not verbose))
             (selector t)
             (stats (ert-run-tests-batch selector)))
        (terpri)
        (let ((passed (ert-stats-completed-expected stats))
              (failed (+ (ert-stats-completed-unexpected stats)
                         (ert-stats-skipped stats)))
              (total (ert-stats-total stats)))
          (if (zerop failed)
              (print! (success "All %d tests passed!" total))
            (print! (error "%d/%d tests failed" failed total)))
          (exit! (if (zerop failed) 0 1)))))))


;;
;;; Helpers

(defun doom-test--find-test-files (targets)
  "Find test files matching TARGETS.
If TARGETS is nil, find all test files in `doom-test-dir'."
  (let (files)
    (if (null targets)
        ;; No targets: find all test files
        (setq files (directory-files-recursively
                     doom-test-dir
                     "^test-.*\\.el$"))
      ;; Process each target
      (dolist (target targets)
        (let ((path (expand-file-name target)))
          (cond
           ;; Exact file path
           ((and (file-exists-p path)
                 (not (file-directory-p path)))
            (push path files))
           ;; Directory
           ((file-directory-p path)
            (setq files (nconc files
                               (directory-files-recursively
                                path "^test-.*\\.el$"))))
           ;; Try as a pattern in test dir
           (t
            (let ((pattern-files
                   (directory-files-recursively
                    doom-test-dir
                    (format "^test-.*%s.*\\.el$" (regexp-quote target)))))
              (setq files (nconc files pattern-files))))))))
    (delete-dups files)))

(provide 'doom-cli-test)
;;; test.el ends here
