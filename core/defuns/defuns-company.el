;;; defuns-company.el

;;;###autoload
(defun doom/company-evil-complete-next (&optional arg)
  (call-interactively 'company-dabbrev)
  (if (eq company-candidates-length 1)
      (company-complete)))

;;;###autoload
(defun doom/company-evil-complete-previous (&optional arg)
  (let ((company-selection-wrap-around t))
    (call-interactively 'company-dabbrev)
    (if (eq company-candidates-length 1)
        (company-complete)
      (call-interactively 'company-select-previous))))

;;;###autoload
(defun doom/company-complete-common-or-complete-full ()
  (interactive)
  (when (company-manual-begin)
    (if (eq last-command #'company-complete-common-or-cycle)
        (let ((company-selection-wrap-around t))
          (call-interactively #'company-complete-selection))
      (let ((buffer-mod-tick (buffer-chars-modified-tick)))
        (call-interactively #'company-complete-common)
        (when (= buffer-mod-tick (buffer-chars-modified-tick))
          (call-interactively #'company-complete-selection)
          (call-interactively #'company-complete))))))

(defun doom--company-whole-lines ()
  (split-string
   (replace-regexp-in-string
    "^[\t\s]+" ""
    (concat (buffer-substring-no-properties (point-min) (line-beginning-position))
            (buffer-substring-no-properties (line-end-position) (point-max))))
   "\\(\r\n\\|[\n\r]\\)" t))

;;;###autoload
(defun doom/company-whole-lines (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (let ((lines (doom--company-whole-lines)))
    (cl-case command
      (interactive (company-begin-backend 'doom/company-whole-lines))
      (prefix (company-grab-line "^[\t\s]*\\(.+\\)" 1))
      (candidates (all-completions arg lines)))))

;;;###autoload
(defun doom/company-dict-or-keywords ()
  (interactive)
  (let ((company-backends '((company-keywords company-dict))))
    (call-interactively 'company-complete)))

;;;###autoload
(defun doom/company-complete ()
  "Bring up the completion popup. If there is only one result, auto-complete it."
  (interactive)
  (require 'company)
  (when (and (bound-and-true-p company-mode)
             (company-manual-begin)
             (= company-candidates-length 1))
    (company-complete-common)))

(provide 'defuns-company)
;;; defuns-company.el ends here
