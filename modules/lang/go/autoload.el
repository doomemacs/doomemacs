;;; lang/go/autoload.el -*- lexical-binding: t; -*-

;;
;; Tests

(defvar +go-test-last nil
  "The last test run.")

(defun +go--spawn (cmd)
  (save-selected-window
    (compile cmd)))

(defun +go--run-tests (args)
  (let ((cmd (concat "go test -test.v " args)))
    (setq +go-test-last (concat "cd " default-directory ";" cmd))
    (+go--spawn cmd)))

;;;###autoload
(defun +go/test-rerun ()
  (interactive)
  (if +go-test-last
      (+go--spawn +go-test-last)
    (+go/test-all)))

;;;###autoload
(defun +go/test-all ()
  (interactive)
  (+go--run-tests ""))

;;;###autoload
(defun +go/test-nested ()
  (interactive)
  (+go--run-tests "./..."))

;;;###autoload
(defun +go/test-single ()
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (save-excursion
        (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
        (+go--run-tests (concat "-run" "='^\\Q" (match-string-no-properties 2) "\\E$'")))
    (error "Must be in a _test.go file")))

;;;###autoload
(defun +go/test-file ()
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (save-excursion
        (goto-char (point-min))
        (let ((func-list))
          (while (re-search-forward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)" nil t)
            (push (match-string-no-properties 2) func-list))
          (+go--run-tests (concat "-run" "='^(" (string-join func-list "|")  ")$'"))))
    (error "Must be in a _test.go file")))

;;;###autoload
(defun +go/bench-all ()
  (interactive)
  (+go--run-tests "-test.run=NONE -test.bench=\".*\""))

;;;###autoload
(defun +go/bench-single ()
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (save-excursion
        (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Benchmark[[:alnum:]_]+\\)(.*)")
        (+go--run-tests (concat "-test.run=NONE -test.bench" "='^\\Q" (match-string-no-properties 2) "\\E$'")))
    (error "Must be in a _test.go file")))


;;;###autoload
(defun +go/play-buffer-or-region (&optional beg end)
  "TODO"
  (interactive "r")
  (if (use-region-p)
      (go-play-region beg end)
    (go-play-buffer)))
