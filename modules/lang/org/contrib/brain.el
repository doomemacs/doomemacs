;;; lang/org/contrib/brain.el -*- lexical-binding: t; -*-
;;;###if (featurep! +brain)

(use-package! org-brain
  :defer t
  :init
  (setq org-brain-visualize-default-choices 'all
        org-brain-title-max-length 24
        org-brain-include-file-entries nil
        org-brain-file-entries-use-title nil)

  :config
  (set-evil-initial-state! 'org-brain-visualize-mode 'emacs)
  (set-popup-rule! "^\\*org-brain" :side 'right :size 1.00 :select t :ttl nil)

  (cl-pushnew '("b" "Brain" plain (function org-brain-goto-end)
                "* %i%?" :empty-lines 1)
              org-capture-templates
              :key #'car :test #'equal))
