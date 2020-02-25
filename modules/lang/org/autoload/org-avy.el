;;; lang/org/autoload/org-avy.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org-headline-avy ()
  "TODO"
  (require 'avy)
  (save-excursion
    (when-let* ((org-reverse-note-order t)
                (pos (avy-with avy-goto-line (avy-jump (rx bol (1+ "*") (1+ blank))))))
      (when (integerp (car pos))
        ;; If avy is aborted with "C-g", it returns `t', so we know it was NOT
        ;; aborted when it returns an int. If it doesn't return an int, we
        ;; return nil.
        (copy-marker (car pos))))))

;;;###autoload
(defun +org/goto-visible ()
  "TODO"
  (interactive)
  (goto-char (+org-headline-avy)))
