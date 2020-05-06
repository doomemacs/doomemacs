;;; lang/org/contrib/present.el -*- lexical-binding: t; -*-
;;;###if (featurep! +present)

(defvar +org-present-text-scale 6
  "The `text-scale-amount' for `org-tree-slide-mode'.")

(after! ox
  (add-to-list 'org-export-backends 'beamer))


;;
;;; Packages

(use-package! org-re-reveal
  :after ox
  :init
  (setq org-re-reveal-root "https://revealjs.com"))


(use-package! org-tree-slide
  :commands org-tree-slide-mode
  :config
  (org-tree-slide-simple-profile)
  (setq org-tree-slide-skip-outline-level 2
        org-tree-slide-activate-message " "
        org-tree-slide-deactivate-message " "
        org-tree-slide-modeline-display nil
        org-tree-slide-heading-emphasis t)

  (add-hook 'org-tree-slide-mode-after-narrow-hook #'org-display-inline-images)
  (add-hook! 'org-tree-slide-mode-hook
             #'+org-present-hide-blocks-h
             #'+org-present-prettify-slide-h)

  (when (featurep! :editor evil)
    (map! :map org-tree-slide-mode-map
          :n [C-right] #'org-tree-slide-move-next-tree
          :n [C-left]  #'org-tree-slide-move-previous-tree)
    (add-hook 'org-tree-slide-mode-hook #'evil-normalize-keymaps))

  (defadvice! +org-present--narrow-to-subtree-a (orig-fn &rest args)
    "Narrow to the target subtree when you start the presentation."
    :around #'org-tree-slide--display-tree-with-narrow
    (letf! ((defun org-narrow-to-subtree ()
              (save-excursion
                (save-match-data
                  (org-with-limited-levels
                   (narrow-to-region
                    (progn
                      (when (org-before-first-heading-p)
                        (org-next-visible-heading 1))
                      (ignore-errors (org-up-heading-all 99))
                      (forward-line 1)
                      (point))
                    (progn (org-end-of-subtree t t)
                           (when (and (org-at-heading-p) (not (eobp)))
                             (backward-char 1))
                           (point))))))))
      (apply orig-fn args))))
