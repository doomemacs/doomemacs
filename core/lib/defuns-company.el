;;; defuns-company.el

;;;###autoload
(defun narf/company-evil-complete-next (&optional arg)
  (call-interactively 'company-dabbrev)
  (if (eq company-candidates-length 1)
      (company-complete)))

;;;###autoload
(defun narf/company-evil-complete-previous (&optional arg)
  (let ((company-selection-wrap-around t))
    (call-interactively 'company-dabbrev)
    (if (eq company-candidates-length 1)
        (company-complete)
      (call-interactively 'company-select-previous))))

;;;###autoload
(defun narf/company-complete-common-or-complete-full ()
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

(defun narf--company-whole-lines ()
  (split-string
   (replace-regexp-in-string
    "^[\t\s]+" ""
    (concat (buffer-substring-no-properties (point-min) (line-beginning-position))
            (buffer-substring-no-properties (line-end-position) (point-max))))
   "\\(\r\n\\|[\n\r]\\)" t))

;;;###autoload
(defun narf/company-whole-lines (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (let ((lines (narf--company-whole-lines)))
    (cl-case command
      (interactive (company-begin-backend 'narf/company-whole-lines))
      (prefix (company-grab-line "^[\t\s]*\\(.+\\)" 1))
      (candidates (all-completions arg lines)))))

(provide 'defuns-company)
;;; defuns-company.el ends here
