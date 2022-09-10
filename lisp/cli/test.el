;;; lisp/cli/test.el -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; The heart of Doom's test DSL and framework. Powered by either ERT or
;; Buttercup, this extends testing frameworks to allow for isolated execution
;; contexts on several levels, a more sophisticated CLI for tests, and
;; integration with Doom's profiles system so testing environments can be
;; generated on-the-fly.
;;
;;; Code:

;;
;;; Variables

;; TODO Implement me
(defvar doom-test-backend 'ert
  "One of `ert' or `buttercup'.")

;; TODO Implement me
(defvar doom-test-isolation-level nil
  "Determines the testing strategy for tests.

Should be one of:

  nil    -- Run all tests in the same session.
  file   -- Run each test file in isolated sessions.
  group  -- Run each group of tests in isolated sessions.
  t      -- Run each individual test in isolated sessions (very slow).")


;;
;;; Commands

;; FIXME Will be fixed in v3.1
(defcli-stub! test
    ((backend ("--ert" "--buttercup"))
     (jobs    ("-j" "--jobs" int))
     &rest targets)
  "Run Doom unit tests.")


;;
;;; Helpers

;; Nothing here yet

(provide 'doom-cli-test)
;;; test.el ends here
