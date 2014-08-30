
(add-hook 'dired-load-hook
          (lambda()
            (use-package dired+ :config
              (setq dired-recursive-deletes 'always
                    dired-recursive-copies 'always

                    ;; if there is a dired buffer displayed in the next window, use its
                    ;; current subdir, instead of the current subdir of this dired buffer
                    dired-dwim-target t))))

(use-package ag :defer t
  :config
  (define-key ag-mode-map [escape] 'kill-buffer-and-window))

(use-package helm :defer t)
(use-package grizzl :defer t)
(use-package neotree :commands (neotree-show neotree-hide neotree-toggle))

(use-package projectile
  :diminish projectile-mode
  :config
  (progn (projectile-global-mode)
         (setq projectile-completion-system 'grizzl
               projectile-enable-caching t)))

(use-package ido
  :init
  (progn
    ;; ido remaps its keys every time it's invoked, this screws with
    ;; custom mappings. So we've gotta neuter ido.
    (defun ido-init-completion-maps ())

    (setq ido-common-completion-map   (make-sparse-keymap))
    (setq ido-file-dir-completion-map (make-sparse-keymap))
    (setq ido-file-completion-map     (make-sparse-keymap))
    (setq ido-buffer-completion-map   (make-sparse-keymap))

    (set-keymap-parent ido-common-completion-map   minibuffer-local-map)
    (set-keymap-parent ido-file-dir-completion-map ido-common-completion-map)
    (set-keymap-parent ido-file-completion-map     ido-file-dir-completion-map)
    (set-keymap-parent ido-buffer-completion-map   ido-common-completion-map))
  :config
  (progn
    (use-package ido-ubiquitous)
    (use-package ido-vertical-mode)
    (use-package flx-ido)

    (ido-mode 1)
    (ido-vertical-mode 1)
    (ido-everywhere 1)
    (ido-ubiquitous-mode 1)
    (flx-ido-mode 1)

    (add-to-list 'ido-ignore-files "\\`.DS_Store\\'")
    (setq ido-use-faces nil
          ido-confirm-unique-completion t
          ido-case-fold t
          ido-enable-tramp-completion nil
          ido-enable-flex-matching t
          ido-create-new-buffer 'always
          ido-enable-tramp-completion t
          ido-enable-last-directory-history t)
    ))

;;
(provide 'init-project)
