;;; lang/org/contrib/present.el -*- lexical-binding: t; -*-
;;;###if (featurep! +present)

(defvar +org-present-text-scale 5
  "The `text-scale-amount' for `org-tree-slide-mode'.")

(defvar +org-present-hide-first-heading nil
  "If non-nil, hide the top-level heading for the current slide.

Some presenters think the first level heading takes up too much space, or use
them as slide names, rather than titles. Instead, you can use second level
headings as titles, and you have more freedom to place them wherever you like.")


(after! ox
  (add-to-list 'org-export-backends 'beamer))


;;
;;; Packages

(use-package! org-re-reveal
  :after ox
  :config
  (setq org-re-reveal-root (expand-file-name "../../" (locate-library "dist/reveal.js" t))
        org-re-reveal-revealjs-version "4"))


(use-package! org-tree-slide
  :commands org-tree-slide-mode
  :config
  (org-tree-slide-simple-profile)
  (setq org-tree-slide-activate-message " "
        org-tree-slide-deactivate-message " "
        org-tree-slide-modeline-display nil
        org-tree-slide-heading-emphasis t)

  (add-hook 'org-tree-slide-after-narrow-hook #'org-display-inline-images)
  (add-hook 'org-tree-slide-mode-hook #'+org-present-prettify-slide-h)
  (add-hook 'org-tree-slide-play-hook #'+org-present-hide-blocks-h)

  (when (featurep! :editor evil)
    (map! :map org-tree-slide-mode-map
          :n [C-right] #'org-tree-slide-move-next-tree
          :n [C-left]  #'org-tree-slide-move-previous-tree)
    (add-hook 'org-tree-slide-mode-hook #'evil-normalize-keymaps))

  (defadvice! +org-present--hide-first-heading-maybe-a (fn &rest args)
    "Omit the first heading if `+org-present-hide-first-heading' is non-nil."
    :around #'org-tree-slide--display-tree-with-narrow
    (letf! (defun org-narrow-to-subtree ()
             (save-excursion
               (save-match-data
                 (org-with-limited-levels
                  (narrow-to-region
                   (progn
                     (when (org-before-first-heading-p)
                       (org-next-visible-heading 1))
                     (ignore-errors (org-up-heading-all 99))
                     (when +org-present-hide-first-heading
                       (forward-line 1))
                     (point))
                   (progn (org-end-of-subtree t t)
                          (when (and (org-at-heading-p) (not (eobp)))
                            (backward-char 1))
                          (point)))))))
      (apply fn args))))
