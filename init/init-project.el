;; Project nav+search tools (projectile, helm, ag)
(use-package neotree
  :commands (neotree-show neotree-hide neotree-toggle neo-global--window-exists-p neotree-dir neotree-find)
  :init
  (progn
    (defun my-neotree-open (&optional dir)
      (interactive)
      (neotree-dir (or dir (project-root))))

    (defun my-neotree-toggle ()
      (interactive)
      (if (neo-global--window-exists-p)
          (neotree-hide)
        (my-neotree-open)))

    (defun my-neotree-find ()
      (interactive)
      (save-excursion (my-neotree-open))
      (neotree-find)))
  :config
  (progn
    (setq neo-create-file-auto-open t
          neo-mode-line-type 'neotree
          neo-persist-show t
          neo-window-width 28
          neo-show-updir-line nil
          neo-auto-indent-point t)
    (add-to-list 'evil-motion-state-modes 'neotree-mode)
    (defun my-neotree-keymap ()
      (bind evil-motion-state-local-map
            "\\\\" 'neotree-hide
            "RET" 'neotree-enter
            "J"   'neotree-select-next-sibling-node
            "K"   'neotree-select-previous-sibling-node
            "H"   'neotree-select-up-node
            "L"   'neotree-select-down-node
            "v"   'neotree-enter-vertical-split
            "s"   'neotree-enter-horizontal-split
            "c"   'neotree-create-node
            "d"   'neotree-delete-node
            "g"   'neotree-refresh
            "q"   'neotree-hide
            "r"   'neotree-rename-node
            "R"   'neotree-change-root
            "?"   'neotree-))

    (add-hook 'neotree-mode-hook 'my-neotree-keymap)))


(provide 'init-project)
;;; init-project.el ends here
