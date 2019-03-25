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
        (cl-destructuring-bind (beg . end)
            (markdown-unwrap-things-in-region
             (region-beginning) (region-end)
             +markdown--regex-del 2 4)
          (markdown-wrap-or-insert delim delim nil beg end))
      ;; Bold markup removal, bold word at point, or empty markup insertion
      (if (thing-at-point-looking-at +markdown--regex-del)
          (markdown-unwrap-thing-at-point nil 2 4)
        (markdown-wrap-or-insert delim delim 'word nil nil)))))

;;;###autoload
(defun +markdown-flyspell-word-p ()
  "Return t if point is on a word that should be spell checked.

Return nil if on a link url, markup, html, or references."
  (let ((faces (doom-enlist (get-text-property (point) 'face))))
    (or (and (memq 'font-lock-comment-face faces)
             (memq 'markdown-code-face faces))
        (not (cl-loop with unsafe-faces = '(markdown-reference-face
                                            markdown-url-face
                                            markdown-markup-face
                                            markdown-comment-face
                                            markdown-html-attr-name-face
                                            markdown-html-attr-value-face
                                            markdown-html-tag-name-face
                                            markdown-code-face)
                      for face in faces
                      if (memq face unsafe-faces)
                      return t)))))
