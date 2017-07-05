;;; org/org-present/config.el -*- lexical-binding: t; -*-

(defvar +org-present-text-scale 7
  "The `text-scale-amount' for `org-tree-slide-mode'.")

(add-hook 'org-load-hook #'+org-present|init t)


;;
;; Plugins
;;

(def-package! ox-reveal
  :config
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/"
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
        [right] #'org-tree-slide-move-next-tree
        [left]  #'org-tree-slide-move-previous-tree)

  (add-hook! 'org-tree-slide-mode-after-narrow-hook
    #'(+org-present|detect-slide +org-present|add-overlays org-display-inline-images))

  (add-hook 'org-tree-slide-mode-hook #'+org-present|org-tree-prepare-window)
  (advice-add #'org-tree-slide--display-tree-with-narrow
              :around #'+doom-present*org-tree-slide-narrow-exclude-header))


(def-package! centered-window-mode
  :commands centered-window-mode
  :config
  (setq cwm-use-vertical-padding t
        cwm-frame-internal-border 110
        cwm-left-fringe-ratio -10
        cwm-centered-window-width 240))


;;
;; Bootstrap
;;

(defun +org-present|init ()
  (require 'ox-reveal)

  (map! :map org-mode-map
        "<f8>" #'+org-present/org-tree-slides
        "<f7>" #'+org-present/next))

