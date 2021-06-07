;;; lang/org/contrib/strike.el -*- lexical-binding: t; -*-
;;;###if (featurep! +strike)

(after! org
  (setq org-fontify-done-headline t)
  (custom-set-faces!
    '(org-done :strike-through t)
    '(org-headline-done :strike-through t)))
