;;; ui/tabs/config.el -*- lexical-binding: t; -*-

(use-package! centaur-tabs
  :after-call after-find-file dired-initial-position-hook
  :init
  (setq centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-set-bar 'left
        centaur-tabs-set-modified-marker t
        centaur-tabs-close-button "✕"
        centaur-tabs-modified-marker "⬤")

  :config
  (add-hook '+doom-dashboard-mode-hook #'centaur-tabs-local-mode)

  (centaur-tabs-mode +1))


;; TODO tab-bar-mode (emacs 27)
;; TODO tab-line-mode (emacs 27)
