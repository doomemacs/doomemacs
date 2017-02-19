;;; company.el

;;;###autoload
(defun +company/complete ()
  "Bring up the completion popup. If only one result, complete it."
  (interactive)
  (require 'company)
  (when (and (company-manual-begin)
             (= company-candidates-length 1))
    (company-complete-common)))

;;;###autoload
(defun +company/whole-lines (command &optional arg &rest ignored)
  "`company-mode' completion backend that completes whole-lines, akin to vim's
C-x C-l."
  (interactive (list 'interactive))
  (require 'company)
  (unless (bound-and-true-p company-mode) (company-mode))
  (let ((lines (split-string
                (replace-regexp-in-string
                 "^[\t\s]+" ""
                 (concat (buffer-substring-no-properties (point-min) (line-beginning-position))
                         (buffer-substring-no-properties (line-end-position) (point-max))))
                "\\(\r\n\\|[\n\r]\\)" t)))
    (cl-case command
      (interactive (company-begin-backend '+company/whole-lines))
      (prefix (company-grab-line "^[\t\s]*\\(.+\\)" 1))
      (candidates (all-completions arg lines)))))

;;;###autoload
(defun +company/dict-or-keywords ()
  "`company-mode' completion combining `company-dict' and `company-keywords'."
  (interactive)
  (require 'company-dict)
  (require 'company-keywords)
  (let ((company-backends '((company-keywords company-dict))))
    (call-interactively 'company-complete)))

