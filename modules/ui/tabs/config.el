;;; ui/tabs/config.el -*- lexical-binding: t; -*-

(use-package! centaur-tabs
  :after-call after-find-file dired-initial-position-hook
  :init
  (when (window-system)
    (setq centaur-tabs-set-bar 'left))
  (setq centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-set-modified-marker t
        centaur-tabs-close-button "✕"
        centaur-tabs-modified-marker "⬤"
        ;; Scrolling (with the mouse wheel) past the end of the tab list
        ;; replaces the tab list with that of another Doom workspace. This
        ;; prevents that.
        centaur-tabs-cycle-scope 'tabs)

  :config
  (add-hook '+doom-dashboard-mode-hook #'centaur-tabs-local-mode)
  (add-hook '+popup-buffer-mode-hook #'centaur-tabs-local-mode)

  (centaur-tabs-mode +1))


;; TODO tab-bar-mode (emacs 27)
;; TODO tab-line-mode (emacs 27)
