;;; ui/tabbar/config.el -*- lexical-binding: t; -*-

;; This is here for reference. I don't use tabbar because it's unstable and not
;; very useful (all it does is show a buffer list on top of *every* window). I
;; find ivy (or helm) or even `buffer-menu' is better suited for this purpose.

(def-package! tabbar
  :hook (doom-init-ui . tabbar-mode)
  :config
  (setq tabbar-use-images nil)

  (defun +tabbar|disable-in-popups ()
    (when (and +popup-buffer-mode tabbar-mode)
      (tabbar-local-mode -1)
      (setq-local header-line-format nil)))
  (add-hook '+popup-buffer-mode-hook #'+tabbar|disable-in-popups)

  (defun +tabbar-display-tab (tab)
    "Return a label for TAB that resembles tabs in Atom."
    (let ((label (if tabbar--buffer-show-groups
                     (format "[%s]" (tabbar-tab-tabset tab))
                   (format "%s" (tabbar-tab-value tab))))
          (bar-color (face-background 'doom-modeline-bar nil t))
          (bar-height 25)
          (bar-width 3)
          (selected-p (eq tab (tabbar-selected-tab (tabbar-current-tabset)))))
      (concat (when (and (display-graphic-p) selected-p)
                (+doom-modeline--make-xpm bar-color bar-height bar-width))
              " "
              (if tabbar-auto-scroll-flag
                  label
                (tabbar-shorten
                 label (max 1 (/ (window-width)
                                 (length (tabbar-view
                                          (tabbar-current-tabset)))))))
              " ")))
  (setq tabbar-tab-label-function #'+tabbar-display-tab))
