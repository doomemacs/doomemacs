;;; ui/vc-gutter/autoload/diff-hl.el -*- lexical-binding: t; -*-
;;;###if (modulep! +diff-hl)

;;;###autoload
(defalias '+vc-gutter/stage-hunk #'diff-hl-stage-current-hunk)
;;;###autoload
(defalias '+vc-gutter/revert-hunk #'diff-hl-revert-hunk)
;;;###autoload
(defalias '+vc-gutter/next-hunk #'diff-hl-next-hunk)
;;;###autoload
(defalias '+vc-gutter/previous-hunk #'diff-hl-previous-hunk)
