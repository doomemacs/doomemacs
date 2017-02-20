;;; emacs/org/autoload/util.el

;;;###autoload
(defun +org-get-property (name &optional file)
  "Get a propery from an org file."
  (save-excursion
    (goto-char 1)
    (re-search-forward (format "^#\\+%s:[ \t]*\\([^\n]+\\)" (upcase name)) nil t)
    (buffer-substring-no-properties (match-beginning 1) (match-end 1))))

