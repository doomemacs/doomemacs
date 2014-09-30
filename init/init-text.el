(provide 'init-text)

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("/README\\'" . markdown-mode))
  :pre-load
  (progn
    ;; Implement strike-through formatting
    (defvar markdown-regex-del "\\(^\\|[^\\]\\)\\(\\(~\\{2\\}\\)\\([^ \n	\\]\\|[^ \n	]\\(?:.\\|\n[^\n]\\)*?[^\\ ]\\)\\(\\3\\)\\)")
    (defun markdown-insert-del ()
      (interactive)
      (let ((delim "~~"))
        (if (markdown-use-region-p)
            ;; Active region
            (let ((bounds (markdown-unwrap-things-in-region
                           (region-beginning) (region-end)
                           markdown-regex-del 2 4)))
              (markdown-wrap-or-insert delim delim nil (car bounds) (cdr bounds)))
          ;; Bold markup removal, bold word at point, or empty markup insertion
          (if (thing-at-point-looking-at markdown-regex-del)
              (markdown-unwrap-thing-at-point nil 2 4)
            (markdown-wrap-or-insert delim delim 'word nil nil)))))))
