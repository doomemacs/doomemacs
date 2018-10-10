;;; lang/org/+brain.el -*- lexical-binding: t; -*-
;;;###if (featurep! +brain)

(def-package! org-brain
  :init (add-to-list 'evil-motion-state-modes 'org-brain-visualize-mode)
  :config
  (add-hook! org-brain-visualize-mode 'visual-line-mode)
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  (setq org-brain-visualize-default-choices 'files)
  (setq org-brain-title-max-length 12)
  (set-popup-rule! "*org-brain*" :ignore t))
