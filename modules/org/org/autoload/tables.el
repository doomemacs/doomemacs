;;; org/org/autoload/tables.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org/table-next-row ()
  "Go to the next row (same column) in the current table."
  (interactive)
  (if (org-at-table-p)
      (org-table-next-row)
    (org-down-element)))

;;;###autoload
(defun +org/table-previous-row ()
  "Go to the previous row (same column) in the current table. Before doing so,
re-align the table if necessary. (Necessary because org-mode has a
`org-table-next-row', but not `org-table-previous-row')"
  (interactive)
  (if (org-at-table-p)
      (progn
        (org-table-maybe-eval-formula)
        (org-table-maybe-recalculate-line)
        (if (and org-table-automatic-realign
                 org-table-may-need-update)
            (org-table-align))
        (let ((col (org-table-current-column)))
          (beginning-of-line 0)
          (when (or (not (org-at-table-p)) (org-at-table-hline-p))
            (beginning-of-line))
          (org-table-goto-column col)
          (skip-chars-backward "^|\n\r")
          (when (org-looking-at-p " ") (forward-char))))
    (org-up-element)))

;;;###autoload
(defun +org/table-next-field ()
  (interactive)
  (if (org-at-table-p) (org-table-next-field) (org-end-of-line)))

;;;###autoload
(defun +org/table-previous-field ()
  (interactive)
  (if (org-at-table-p) (org-table-previous-field) (org-beginning-of-line)))

;;;###autoload
(defun +org/table-append-field-or-shift-right ()
  (interactive)
  (org-shiftmetaright)
  (when (org-at-table-p) (org-metaright)))

;;;###autoload
(defun +org/table-prepend-field-or-shift-left ()
  (interactive)
  (if (org-at-table-p) (org-shiftmetaright) (org-shiftmetaleft)))

;;;###autoload
(defun +org/table-append-row-or-shift-down ()
  (interactive)
  (org-shiftmetadown)
  (when (org-at-table-p) (org-metadown)))

;;;###autoload
(defun +org/table-prepend-row-or-shift-up ()
  (interactive)
  (if (org-at-table-p)
      (org-shiftmetadown)
    (org-shiftmetaup)))

