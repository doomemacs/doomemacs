;;; lang/ocaml/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +ocaml/comment-indent-new-line (&optional _)
  "Break line at point and indent, continuing comment if within one."
  (interactive)
  (comment-indent-new-line)
  (when (eq (char-before) ?*)
    (just-one-space))
  (unless (eq (char-after) 32)
    (save-excursion (insert " "))))
