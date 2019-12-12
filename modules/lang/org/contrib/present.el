;;; lang/org/contrib/present.el -*- lexical-binding: t; -*-
;;;###if (featurep! +present)

(defvar +org-present-text-scale 7
  "The `text-scale-amount' for `org-tree-slide-mode'.")

(after! ox
  (add-to-list 'org-export-backends 'beamer))


;;
;;; Packages

(use-package! org-re-reveal
  :after ox
  :init
  (setq org-re-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@3/"))


(use-package! org-tree-slide
  :commands org-tree-slide-mode
  :config
  (org-tree-slide-simple-profile)
  (setq org-tree-slide-skip-outline-level 2
        org-tree-slide-activate-message " "
        org-tree-slide-deactivate-message " "
        org-tree-slide-modeline-display nil)

  (map! :map org-tree-slide-mode-map
        :n [right] #'org-tree-slide-move-next-tree
        :n [left]  #'org-tree-slide-move-previous-tree)

  (add-hook! 'org-tree-slide-mode-after-narrow-hook
             #'+org-present-detect-slide-h
             #'+org-present-add-overlays-h
             #'org-display-inline-images)

  (add-hook 'org-tree-slide-mode-hook #'+org-present-init-org-tree-window-h)

  (defadvice! +org-present--narrow-to-subtree-a (orig-fn &rest args)
    "Narrow to the target subtree when you start the presentation."
    :around #'org-tree-slide--display-tree-with-narrow
    (cl-letf (((symbol-function #'org-narrow-to-subtree)
               (lambda () (save-excursion
                            (save-match-data
                              (org-with-limited-levels
                               (narrow-to-region
                                (progn (org-back-to-heading t)
                                       (forward-line 1)
                                       (point))
                                (progn (org-end-of-subtree t t)
                                       (when (and (org-at-heading-p) (not (eobp))) (backward-char 1))
                                       (point)))))))))
      (apply orig-fn args))))
