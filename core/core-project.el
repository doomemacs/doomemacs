;;; core-project.el --- all your (basic) project navigational needs

(use-package ido
  :defines (flx-ido-mode ido-ubiquitous-debug-mode ido-context-switch-command ido-temp-list)
  :functions (ido-to-end)
  :commands (ido-mode ido-everywhere ido-vertical-mode
             flx-ido-mode ido-ubiquitous-mode ido-find-file
             ido-find-file-in-dir)
  :init
  (setq ido-ignore-buffers
        '("\\` " "^\\*ESS\\*" "^\\*Messages\\*" "^\\*Help\\*" "^\\*Buffer"
          "^\\*.*Completions\\*$" "^\\*Ediff" "^\\*tramp" "^\\*cvs-"
          "_region_" " output\\*$" "^TAGS$" "^\*Ido")
        ido-use-faces nil
        ido-confirm-unique-completion t
        ido-case-fold t
        ido-enable-tramp-completion nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-enable-tramp-completion t
        ido-enable-last-directory-history t
        ido-save-directory-list-file (! (concat narf-temp-dir "ido.last")))
  :config
  (add-to-list 'ido-ignore-files "\\`.DS_Store$")
  (add-to-list 'ido-ignore-files "Icon\\?$")
  (add-hook! ido-setup
    (bind! :map (ido-completion-map ido-file-completion-map)
           "C-w" 'ido-delete-backward-word-updir))

  (ido-mode 1)
  (ido-everywhere 1)

  (require 'ido-vertical-mode)
  (ido-vertical-mode 1)

  (require 'flx-ido)
  (flx-ido-mode 1)

  (require 'ido-ubiquitous)
  (ido-ubiquitous-mode 1)

  (advice-add 'ido-sort-mtime :override 'narf*ido-sort-mtime)
  (add-hook! (ido-make-file-list ido-make-dir-list) 'narf*ido-sort-mtime)
  (add-hook! ido-setup 'narf|ido-setup-home-keybind))

(use-package neotree
  :commands (neotree-show
             neotree-hide
             neotree-toggle
             neotree-dir
             neotree-find
             neo-global--window-exists-p)
  :functions (neo-buffer--unlock-width neo-buffer--lock-width)
  :init
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
  :config
  (defun narf|neotree-init-keymap ()
    (bind! :map evil-motion-state-local-map
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
           "R"   'neotree-change-root))

  (add-hook! neotree-mode 'narf|neotree-init-keymap)
  (add-hook! window-configuration-change 'narf|neotree-close-on-window-change)

  (evil-set-initial-state 'neotree-mode 'motion)
  (after! projectile
    (setq projectile-switch-project-action 'neotree-projectile-action))

  (advice-add 'neo-buffer--insert-fold-symbol :override 'narf*neo-buffer-fold-symbol))

(provide 'core-project)
;;; core-project.el ends here
