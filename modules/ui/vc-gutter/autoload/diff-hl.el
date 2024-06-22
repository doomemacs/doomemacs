;;; ui/vc-gutter/autoload/diff-hl.el -*- lexical-binding: t; -*-

;;;###autoload
(defalias '+vc-gutter/stage-hunk #'diff-hl-stage-current-hunk)
;;;###autoload
(defalias '+vc-gutter/next-hunk #'diff-hl-next-hunk)
;;;###autoload
(defalias '+vc-gutter/previous-hunk #'diff-hl-previous-hunk)

(defvar vc-suppress-confirm)
;;;###autoload
(defun +vc-gutter/revert-hunk (&optional no-prompt)
  "Invoke `diff-hl-revert-hunk'."
  (interactive "P")
  (let ((vc-suppress-confirm (if no-prompt t)))
    (call-interactively #'diff-hl-revert-hunk)))

;;;###autoload
(defun +vc-gutter/save-and-revert-hunk ()
  "Invoke `diff-hl-revert-hunk' with `vc-suppress-confirm' set."
  (interactive)
  (+vc-gutter/revert-hunk t))
