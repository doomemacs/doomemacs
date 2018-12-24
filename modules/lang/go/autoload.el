;;; lang/go/autoload.el -*- lexical-binding: t; -*-

;;
;; Tests

(defvar +go-test-last nil
  "The last test run.")

(defun +go--run-tests (args)
  (require 'async)
  (save-selected-window
    (async-shell-command (concat "go test " args))))

;;;###autoload
(defun +go/test-rerun ()
  (interactive)
  (if +go-test-last
      (funcall +go-test-last)
    (+go/run-all-tests)))

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
        (+go--run-tests (concat "-run" "='" (match-string-no-properties 2) "'")))
    (error "Must be in a _test.go file")))

;;;###autoload
(defun +go/play-buffer-or-region (&optional beg end)
  "TODO"
  (interactive "r")
  (if (use-region-p)
      (go-play-region beg end)
    (go-play-buffer)))
