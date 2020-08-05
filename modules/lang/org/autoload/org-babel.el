;;; lang/org/autoload/org-babel.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org-eval-handler (beg end)
  "TODO"
  (save-excursion
    (if (not (cl-loop for pos in (list beg (point) end)
                      if (save-excursion (goto-char pos) (org-in-src-block-p t))
                      return (goto-char pos)))
        (message "Nothing to evaluate at point")
      (org-babel-where-is-src-block-head)
      (let ((beg (max beg (match-beginning 5)))
            (end (min end (match-end 5)))
            (major-mode
             (org-src-get-lang-mode (or (org-eldoc-get-src-lang)
                                        (user-error "No lang specified for this src block")))))
        (+eval/region beg end)))))


;;
;;; Hooks

;;;###autoload
(defun +org-clear-babel-results-h ()
  "Remove the results block for the org babel block at point."
  (when (and (org-in-src-block-p t)
             (org-babel-where-is-src-block-result))
    (org-babel-remove-result)
    t))
