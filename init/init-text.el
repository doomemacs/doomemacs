(add-hook 'text-mode-hook 'narf|enable-hard-wrap)
(add-hook 'prog-mode-hook 'narf|enable-comment-hard-wrap)

(use-package markdown-mode
  :mode (("\\.md$"   . markdown-mode)
         ("/README$" . markdown-mode))
  :functions (markdown-use-region-p markdown-unwrap-things-in-region
              markdown-wrap-or-insert markdown-unwrap-thing-at-point)
  :init
  ;; Implement strike-through formatting
  (defvar markdown-regex-del "\\(^\\|[^\\]\\)\\(\\(~\\{2\\}\\)\\([^ \n	\\]\\|[^ \n	]\\(?:.\\|\n[^\n]\\)*?[^\\ ]\\)\\(\\3\\)\\)")
  :config
  (progn
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
            (markdown-wrap-or-insert delim delim 'word nil nil)))))

    (sp-local-pair 'markdown-mode "*" "*"
                   :unless '(sp-point-after-bol-p sp-point-before-same-p sp-point-after-same-p))

    (bind :map markdown-mode-map
          "<backspace>"  nil
          "<M-left>"     nil
          "<M-right>"    nil

          "M-*"  'markdown-insert-list-item
          "M-b"  'markdown-insert-bold
          "M-i"  'markdown-insert-italic
          "M-`"  'markdown-insert-del

          :normal :visual
          ",i"   'markdown-insert-image
          ",l"   'markdown-insert-link
          ",L"   'markdown-insert-reference-link-dwim
          ",b"   'markdown-preview

          :normal
          "[p"   'markdown-promote
          "]p"   'markdown-demote

          :insert
          "M--"  'markdown-insert-hr)))


(provide 'init-text)
;;; init-text.el ends here
