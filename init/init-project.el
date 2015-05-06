;; Project nav+search tools (projectile, helm, ag)
(use-package neotree
  :commands (neotree-show neotree-hide neotree-toggle)
  :config
  (progn (setq neo-create-file-auto-open t
               neo-smart-open t
               neo-persist-show nil)
         (add-hook! 'neotree-mode-hook
                    (setq mode-line-format nil)
                    (bind evil-motion-state-local-map
                          (kbd "TAB") 'neotree-enter
                          (kbd "RET") 'neotree-enter))))


;; (use-package dired
;;   :disabled t
;;   :config
;;   (progn
;;     (setq dired-recursive-deletes 'always
;;           dired-recursive-copies 'always
;;           dired-auto-revert-buffer t

;;           ;; if there is a dired buffer displayed in the next
;;           ;; window, use its current subdir, instead of the
;;           ;; current subdir of this dired buffer
;;           dired-dwim-target t)

;;     (push '(dired-mode :position bottom :height 0.5 :stick t) popwin:special-display-config)

;;     (add-hook! 'dired-mode-hook
;;       (use-package 'dired+ :config (toggle-diredp-find-file-reuse-dir 1)))))


(provide 'init-project)
;;; init-project.el ends here
