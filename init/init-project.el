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
      (neotree-find))
    (add-hook 'neotree-mode-hook 'my-neotree-keymap))
  :config
  (progn
    (setq neo-create-file-auto-open t
          neo-mode-line-type 'none
          neo-persist-show t
          neo-window-width 22
          neo-show-updir-line nil
          neo-auto-indent-point t
          neo-banner-message nil
          ;; requires <https://github.com/jeffplang/emacs-neotree> fork of
          ;; neotree (at least, until the PR is accepted). Causes neotree to
          ;; open in a vertical split that consumes the entire height of the
          ;; frame.
          neo-modern-sidebar t)

    ;; Custom ascii theme
    (defun neo-buffer--insert-fold-symbol (name)
      (let ((n-insert-symbol (lambda (n)
                               (neo-buffer--insert-with-face
                                n 'neo-expand-btn-face))))
        (or (and (equal name 'open)  (funcall n-insert-symbol "- "))
            (and (equal name 'close) (funcall n-insert-symbol "> "))
            (and (equal name 'leaf)  (funcall n-insert-symbol "  ")))))

    ;; Close neotree on window changes, to prevent ensuing mindbuggery
    (add-hook! 'window-configuration-change-hook
               (unless (and (neo-global--window-exists-p)
                            (eq (current-buffer) (neo-global--get-buffer)))
                 (neotree-hide)))

    (after "projectile"
      (setq projectile-switch-project-action 'neotree-projectile-action))
    (add-to-list 'evil-motion-state-modes 'neotree-mode)
    (defun my-neotree-keymap ()
      (bind evil-motion-state-local-map
            "ESC"  'neotree-hide
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
            "?"   'neotree-))))


(provide 'init-project)
;;; init-project.el ends here
