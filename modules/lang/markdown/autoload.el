;;; lang/markdown/autoload.el -*- lexical-binding: t; -*-

;; Implement strike-through formatting
(defvar +markdown--regex-del
  "\\(^\\|[^\\]\\)\\(\\(~\\{2\\}\\)\\([^ \n	\\]\\|[^ \n	]\\(?:.\\|\n[^\n]\\)*?[^\\ ]\\)\\(\\3\\)\\)")

;;;###autoload
(defun +markdown/insert-del ()
  "Surround region in github strike-through delimiters."
  (interactive)
  (let ((delim "~~"))
    (if (markdown-use-region-p)
        ;; Active region
        (cl-destructuring-bind (beg end)
            (markdown-unwrap-things-in-region
             (region-beginning) (region-end)
             +markdown--regex-del 2 4)
          (markdown-wrap-or-insert delim delim nil beg end))
      ;; Bold markup removal, bold word at point, or empty markup insertion
      (if (thing-at-point-looking-at +markdown--regex-del)
          (markdown-unwrap-thing-at-point nil 2 4)
        (markdown-wrap-or-insert delim delim 'word nil nil)))))
