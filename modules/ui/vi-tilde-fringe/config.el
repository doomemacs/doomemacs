;;; ui/vi-tilde-fringe/config.el -*- lexical-binding: t; -*-

;; indicators for empty lines past EOF
(def-package! vi-tilde-fringe
  :commands vi-tilde-fringe-mode
  :hook (doom-init-ui . global-vi-tilde-fringe-mode)
  :config
  (add-hook! +doom-dashboard-mode
    (when (bound-and-true-p vi-tilde-fringe-mode)
      (vi-tilde-fringe-mode -1))))

