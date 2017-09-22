;;; ui/hl-todo/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +hl-todo|use-face-detection ()
  "Use a different, more primitive method of locating todo keywords.

This is useful for major modes that don't use or have a valid syntax-table entry
for comment start/end characters."
  (set (make-local-variable 'hl-todo-keywords)
       '(((lambda (limit)
            (let (case-fold-search)
              (and (re-search-forward hl-todo-regexp limit t)
                   (memq 'font-lock-comment-face (doom-enlist (get-text-property (point) 'face))))))
          (1 (hl-todo-get-face) t t))))
  (when hl-todo-mode
    (hl-todo-mode -1)
    (hl-todo-mode +1)))

