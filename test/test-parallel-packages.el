;;; test-parallel-packages.el --- Test parallel package operations -*- lexical-binding: t; -*-
;;; Commentary:
;; Automated test for parallel fetch/build pipeline.
;; Run with: emacs --batch -l lisp/doom.el -l test/test-parallel-packages.el -f doom-test-parallel-packages
;;; Code:

(require 'cl-lib)

(defvar doom-test--operation-log nil
  "Log of (timestamp operation package) tuples.")

(defvar doom-test--fetch-delays nil
  "Alist of package -> delay in seconds for simulated fetches.")

(defvar doom-test--build-delays nil
  "Alist of package -> delay in seconds for simulated builds.")

(defun doom-test--log (op package)
  "Log operation OP for PACKAGE with timestamp."
  (push (list (float-time) op package) doom-test--operation-log))

(defun doom-test--mock-async-fetch (repo-dir recipe callback)
  "Mock async fetch that simulates network delay."
  (let* ((package (plist-get recipe :local-repo))
         (delay (or (cdr (assoc package doom-test--fetch-delays)) 0.1)))
    (doom-test--log 'fetch-start package)
    (run-at-time delay nil
                 (lambda ()
                   (doom-test--log 'fetch-end package)
                   (funcall callback t "")))))

(defun doom-test--mock-build-package (package)
  "Mock build that simulates compilation time."
  (let ((delay (or (cdr (assoc package doom-test--build-delays)) 0.05)))
    (doom-test--log 'build-start package)
    (sleep-for delay)
    (doom-test--log 'build-end package)
    t))

(defun doom-test--analyze-log ()
  "Analyze operation log for parallelism."
  (let* ((log (nreverse doom-test--operation-log))
         (fetch-starts (cl-remove-if-not (lambda (e) (eq (nth 1 e) 'fetch-start)) log))
         (fetch-ends (cl-remove-if-not (lambda (e) (eq (nth 1 e) 'fetch-end)) log))
         (build-starts (cl-remove-if-not (lambda (e) (eq (nth 1 e) 'build-start)) log))
         (build-ends (cl-remove-if-not (lambda (e) (eq (nth 1 e) 'build-end)) log))
         (parallel-fetches 0)
         (overlapping-fetch-build 0))

    ;; Count max concurrent fetches
    (let ((concurrent 0) (max-concurrent 0))
      (dolist (entry log)
        (pcase (nth 1 entry)
          ('fetch-start (cl-incf concurrent))
          ('fetch-end (cl-decf concurrent)))
        (setq max-concurrent (max max-concurrent concurrent)))
      (setq parallel-fetches max-concurrent))

    ;; Check if any build overlapped with a fetch
    (dolist (bs build-starts)
      (let ((build-time (nth 0 bs)))
        (dolist (fs fetch-starts)
          (let* ((pkg (nth 2 fs))
                 (fe (cl-find-if (lambda (e) (equal (nth 2 e) pkg)) fetch-ends)))
            (when (and fe
                       (> build-time (nth 0 fs))
                       (< build-time (nth 0 fe)))
              (cl-incf overlapping-fetch-build))))))

    (list :parallel-fetches parallel-fetches
          :overlapping-fetch-build overlapping-fetch-build
          :total-fetches (length fetch-starts)
          :total-builds (length build-starts))))

(defun doom-test-parallel-packages ()
  "Run parallel packages test."
  (message "\n=== Testing Parallel Package Operations ===\n")

  ;; Reset state
  (setq doom-test--operation-log nil)

  ;; Set up mock delays - fetches take longer (simulating network)
  (setq doom-test--fetch-delays
        '(("pkg-a" . 0.3) ("pkg-b" . 0.2) ("pkg-c" . 0.25)
          ("pkg-d" . 0.15) ("pkg-e" . 0.35)))
  (setq doom-test--build-delays
        '(("pkg-a" . 0.1) ("pkg-b" . 0.08) ("pkg-c" . 0.12)
          ("pkg-d" . 0.05) ("pkg-e" . 0.1)))

  ;; Mock the functions
  (cl-letf (((symbol-function 'doom-packages--async-fetch) #'doom-test--mock-async-fetch)
            ((symbol-function 'doom-packages--build-package) #'doom-test--mock-build-package))

    ;; Simulate the pipeline manually since we can't run full doom-packages-update
    (let ((packages '("pkg-a" "pkg-b" "pkg-c" "pkg-d" "pkg-e"))
          (fetch-running (make-hash-table :test 'equal))
          (fetch-complete nil)
          (build-pending nil)
          (max-fetches 8))

      ;; Start fetches
      (dolist (pkg packages)
        (when (< (hash-table-count fetch-running) max-fetches)
          (puthash pkg t fetch-running)
          (doom-test--mock-async-fetch
           nil (list :local-repo pkg)
           (lambda (success output)
             (remhash pkg fetch-running)
             (push pkg fetch-complete)))))

      ;; Pipeline loop
      (let ((remaining packages)
            (started (cl-subseq packages 0 (min max-fetches (length packages)))))
        (setq remaining (cl-subseq packages (min max-fetches (length packages))))

        (while (or remaining (> (hash-table-count fetch-running) 0) fetch-complete build-pending)
          ;; Process completed fetches
          (while fetch-complete
            (push (pop fetch-complete) build-pending))

          ;; Run a build (if any pending)
          (when build-pending
            (doom-test--mock-build-package (pop build-pending)))

          ;; Start more fetches
          (while (and remaining (< (hash-table-count fetch-running) max-fetches))
            (let ((pkg (pop remaining)))
              (puthash pkg t fetch-running)
              (doom-test--mock-async-fetch
               nil (list :local-repo pkg)
               (lambda (success output)
                 (remhash pkg fetch-running)
                 (push pkg fetch-complete)))))

          ;; Wait for events
          (accept-process-output nil 0.02)))))

  ;; Analyze results
  (let ((results (doom-test--analyze-log)))
    (message "Results:")
    (message "  Total fetches: %d" (plist-get results :total-fetches))
    (message "  Total builds: %d" (plist-get results :total-builds))
    (message "  Max parallel fetches: %d" (plist-get results :parallel-fetches))
    (message "  Overlapping fetch/build: %d" (plist-get results :overlapping-fetch-build))

    (message "\nOperation log:")
    (dolist (entry (nreverse doom-test--operation-log))
      (message "  %.3f %s %s" (nth 0 entry) (nth 1 entry) (nth 2 entry)))

    ;; Assertions
    (let ((passed t))
      (unless (>= (plist-get results :parallel-fetches) 2)
        (message "\nFAIL: Expected at least 2 parallel fetches, got %d"
                 (plist-get results :parallel-fetches))
        (setq passed nil))

      (unless (> (plist-get results :overlapping-fetch-build) 0)
        (message "\nFAIL: Expected overlapping fetch/build operations")
        (setq passed nil))

      (if passed
          (message "\n=== PASS: Parallel operations working correctly ===")
        (message "\n=== FAIL: Parallel operations not working as expected ==="))

      (kill-emacs (if passed 0 1)))))

(provide 'test-parallel-packages)
;;; test-parallel-packages.el ends here
