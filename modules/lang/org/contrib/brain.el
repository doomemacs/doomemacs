;;; lang/org/contrib/brain.el -*- lexical-binding: t; -*-
;;;###if (modulep! +brain)

(use-package! org-brain
  :defer t
  :init
  (setq org-brain-visualize-default-choices 'all
        org-brain-title-max-length 24
        org-brain-include-file-entries nil
        org-brain-file-entries-use-title nil)

  :config
  (set-popup-rule! "^\\*org-brain"
    :side 'right :size 1.00 :select t :quit nil :ttl nil)

  (cl-pushnew '("b" "Brain" plain (function org-brain-goto-end)
                "* %i%?" :empty-lines 1)
              org-capture-templates
              :key #'car :test #'equal)

  (when (modulep! :editor evil +everywhere)
    ;; TODO Make a proper evil keybind scheme for org-brain
    ;; REVIEW This should be handled upstream by evil-collection
    (set-evil-initial-state!
      '(org-brain-visualize-mode
        org-brain-select-map
        org-brain-move-map
        org-brain-polymode-map)
      'normal)
    (defun +org--evilify-map (map)
      (let (keys)
        (map-keymap (lambda (event function)
                      (push function keys)
                      (push (vector event) keys))
                    map)
        (apply #'evil-define-key* 'normal map keys)))

    (+org--evilify-map org-brain-visualize-mode-map)
    (+org--evilify-map org-brain-select-map)
    (+org--evilify-map org-brain-move-map)
    (after! polymode
      (+org--evilify-map org-brain-polymode-map))))
