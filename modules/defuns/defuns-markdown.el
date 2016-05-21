;;; defuns-markdown.el

;; Implement strike-through formatting
(defvar doom--markdown-regex-del
  "\\(^\\|[^\\]\\)\\(\\(~\\{2\\}\\)\\([^ \n	\\]\\|[^ \n	]\\(?:.\\|\n[^\n]\\)*?[^\\ ]\\)\\(\\3\\)\\)")

;;;###autoload
(defun doom/markdown-insert-del ()
  "Surround region in github strike-through delimiters."
  (interactive)
  (let ((delim "~~"))
    (if (markdown-use-region-p)
        ;; Active region
        (let ((bounds (markdown-unwrap-things-in-region
                       (region-beginning) (region-end)
                       doom--markdown-regex-del 2 4)))
          (markdown-wrap-or-insert delim delim nil (car bounds) (cdr bounds)))
      ;; Bold markup removal, bold word at point, or empty markup insertion
      (if (thing-at-point-looking-at doom--markdown-regex-del)
          (markdown-unwrap-thing-at-point nil 2 4)
        (markdown-wrap-or-insert delim delim 'word nil nil)))))

(provide 'defuns-markdown)
;;; defuns-markdown.el ends here
