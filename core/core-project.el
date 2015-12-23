;;; core-project.el --- all your (basic) project navigational needs

(use-package ido
  :defines (flx-ido-mode ido-ubiquitous-debug-mode ido-context-switch-command ido-temp-list)
  :functions (ido-to-end)
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
        ido-cr+-max-items 10000
        ido-save-directory-list-file (concat narf-temp-dir "ido.last"))
  :config
  (add-to-list 'ido-ignore-files "\\`.DS_Store$")
  (add-to-list 'ido-ignore-files "Icon\\?$")

  (add-hook! ido-setup
    (require 'ido-vertical-mode)
    (ido-vertical-mode 1)
    (require 'flx-ido)
    (flx-ido-mode 1)
    (map! :map (ido-common-completion-map
                ido-completion-map
                ido-file-completion-map)
          "C-n" 'ido-next-match
          "C-p" 'ido-prev-match
          "C-w" 'ido-delete-backward-word-updir
          "C-u" 'ido-up-directory))

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
        neo-auto-indent-point t
        neo-mode-line-type 'none
        neo-persist-show nil
        neo-window-width 22
        neo-show-updir-line nil
        neo-auto-indent-point t
        neo-banner-message nil)
  :config
  (evil-set-initial-state 'neotree-mode 'motion)
  (add-hook! neotree-mode 'narf|neotree-init-keymap)
  (defun narf|neotree-init-keymap ()
    (map! :map evil-motion-state-local-map
          "ESC"  'neotree-hide
          "q"   'neotree-hide

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
          "r"   'neotree-rename-node
          "R"   'neotree-change-root))

  ;; (add-hook! window-configuration-change 'narf|neotree-close-on-window-change)

  (after! projectile
    (setq projectile-switch-project-action 'neotree-projectile-action))

  ;; A custom and simple theme for neotree
  (advice-add 'neo-buffer--insert-fold-symbol :override 'narf*neo-buffer-fold-symbol))

(provide 'core-project)
;;; core-project.el ends here
