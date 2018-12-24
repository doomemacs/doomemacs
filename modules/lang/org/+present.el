;;; lang/org/+present.el -*- lexical-binding: t; -*-

(add-hook 'org-load-hook #'+org|init-present)

(defvar +org-present-text-scale 7
  "The `text-scale-amount' for `org-tree-slide-mode'.")


;;
;; Packages

(def-package! ox-reveal
  :defer t
  :config
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@3/"
        org-reveal-mathjax t))


(def-package! org-tree-slide
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
    #'(+org-present|detect-slide +org-present|add-overlays org-display-inline-images))

  (add-hook 'org-tree-slide-mode-hook #'+org-present|init-org-tree-window)
  (advice-add #'org-tree-slide--display-tree-with-narrow
              :around #'+org-present*narrow-to-subtree))


(def-package! centered-window :commands centered-window-mode)


;;
;; Bootstrap

(defun +org|init-present ()
  (require 'ox-reveal))
