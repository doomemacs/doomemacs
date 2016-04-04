;;; core-project.el --- all your (basic) project navigational needs

;;
;; Dired
;;

;; Always copy/delete recursively
(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))

;; Auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(setq dired-omit-mode t)

;; List directories first
(defun narf|dired-sort ()
  "Dired sort hook to list directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))
(add-hook 'dired-after-readin-hook 'narf|dired-sort)

;; Automatically create missing directories when creating new files
(defun narf|create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))
(add-to-list 'find-file-not-found-functions #'narf|create-non-existent-directory)

;;
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
        ido-save-directory-list-file (concat narf-temp-dir "/ido.last"))
  :config
  (add-hook! ido-setup
    (add-to-list 'ido-ignore-files "\\`.DS_Store$")
    (add-to-list 'ido-ignore-files "Icon\\?$")
    (advice-add 'ido-sort-mtime :override 'narf*ido-sort-mtime)

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

  (add-hook! (ido-make-file-list ido-make-dir-list) 'narf*ido-sort-mtime)
  (add-hook! ido-setup 'narf|ido-setup-home-keybind))

;;
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
        neo-window-width 27
        neo-show-updir-line nil
        neo-auto-indent-point t
        neo-banner-message nil)
  :config
  (evil-set-initial-state 'neotree-mode 'motion)
  (add-hook! neotree-mode 'narf|neotree-init-keymap)
  (defun narf|neotree-init-keymap ()
    (map! :map evil-motion-state-local-map
          "ESC"    'neotree-hide
          [escape] 'neotree-hide
          "q"      'neotree-hide

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

  (after! projectile
    (setq projectile-switch-project-action 'neotree-projectile-action))

  ;; Don't ask for confirmation when creating files
  (defun narf*neotree-create-node (orig-fun &rest args)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (apply orig-fun args)))
  (advice-add 'neotree-create-node :around 'narf*neotree-create-node)

  ;; Shorter file names for org files
  (defun narf*neo-path--file-short-name (orig-fun &rest args)
    (let ((file (car args)))
      (if (f-ext? file "org")
          (s-replace "-" " " (f-base file))
        (apply orig-fun args))))
  (advice-add 'neo-path--file-short-name :around 'narf*neo-path--file-short-name)

  (advice-add 'narf--evil-window-move  :before 'narf|neotree-close-on-window-change)
  (advice-add 'narf--evil-swap-windows :before 'narf|neotree-close-on-window-change)

  ;; A custom and simple theme for neotree
  (advice-add 'neo-buffer--insert-fold-symbol :override 'narf*neo-buffer-fold-symbol))

(use-package projectile
  :config
  (setq projectile-require-project-root nil
        projectile-enable-caching t
        projectile-cache-file (concat narf-temp-dir "/projectile.cache")
        projectile-known-projects-file (concat narf-temp-dir "/projectile.projects")
        projectile-indexing-method 'alien
        projectile-project-root-files narf-project-root-files
        projectile-file-exists-remote-cache-expire nil)

  (push "ido.last" projectile-globally-ignored-files)
  (push "assets"   projectile-globally-ignored-directories)
  (push ".cask"    projectile-globally-ignored-directories)
  (push ".export"  projectile-globally-ignored-directories)
  (push ".attach"  projectile-globally-ignored-directories)
  (push '("scss" "css") projectile-other-file-alist)
  (push '("css" "scss") projectile-other-file-alist)

  (projectile-global-mode +1))

(provide 'core-project)
;;; core-project.el ends here
