(provide 'init-text)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook 'enable-comment-hard-wrap)

(use-package markdown-mode
  :mode (("\\.md$"   . markdown-mode)
         ("/README$" . markdown-mode))
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
            (markdown-wrap-or-insert delim delim 'word nil nil))))))
  :config
  (let ((map markdown-mode-map))
    (bind '(normal visual) map
          ",i" 'markdown-insert-image
          ",l" 'markdown-insert-link
          ",L" 'markdown-insert-reference-link-dwim
          ",b" 'markdown-preview)

    (bind 'normal map
          "[p" 'markdown-promote
          "]p" 'markdown-demote)

    (bind 'insert map
          (kbd "M--") 'markdown-insert-hr)

    (bind map
          (kbd "<backspace>")  nil
          (kbd "<M-left>")     nil
          (kbd "<M-right>")    nil

          (kbd "s-*") 'markdown-insert-list-item
          (kbd "s-b") 'markdown-insert-bold
          (kbd "s-i") 'markdown-insert-italic
          (kbd "s-`") 'markdown-insert-del)))
