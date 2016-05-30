;;; core-project.el --- all your (basic) project navigational needs

;;
;; Dired
;;

(setq ;; Always copy/delete recursively
      dired-recursive-copies  'always
      dired-recursive-deletes 'top
      ;; Auto refresh dired, but be quiet about it
      global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(add-hook 'dired-mode-hook 'dired-omit-mode)

;; List directories first
(defun doom|dired-sort ()
  "Dired sort hook to list directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))
(add-hook 'dired-after-readin-hook 'doom|dired-sort)

;; Automatically create missing directories when creating new files
(defun doom|create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))
(push 'doom|create-non-existent-directory find-file-not-found-functions)

;;
(use-package ido
  :functions (ido-to-end)
  :init
  (setq ido-ignore-buffers
        '("\\` " "^\\*ESS\\*" "^\\*Messages\\*" "^\\*Help\\*" "^\\*Buffer"
          "^\\*.*Completions\\*$" "^\\*Ediff" "^\\*tramp" "^\\*cvs-"
          "_region_" " output\\*$" "^TAGS$" "^\*Ido")
        ido-use-faces nil
        ido-confirm-unique-completion t
        ido-case-fold t
        ido-create-new-buffer 'always
        ido-enable-flex-matching t
        ido-enable-last-directory-history t
        ido-enable-tramp-completion nil
        ido-enable-tramp-completion t
        ido-cr+-max-items 10000
        ido-save-directory-list-file (concat doom-temp-dir "/ido.last"))
  (add-hook 'ido-setup-hook 'doom|ido-setup-home-keybind)
  :config
  (add-hook! ido-setup
    (push "\\`.DS_Store$" ido-ignore-files)
    (push "Icon\\?$" ido-ignore-files)
    (advice-add 'ido-sort-mtime :override 'doom*ido-sort-mtime)

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

  (add-hook! (ido-make-file-list ido-make-dir-list) 'doom*ido-sort-mtime))

;;
(use-package neotree
  :commands (neotree-show
             neotree-hide
             neotree-toggle
             neotree-dir
             neotree-find
             neo-global--window-exists-p)
  :init
  (setq neo-create-file-auto-open t
        neo-auto-indent-point t
        neo-mode-line-type 'none
        neo-persist-show nil
        neo-window-width 28
        neo-show-updir-line nil
        neo-auto-indent-point t
        neo-theme 'nerd ; fallback
        neo-banner-message nil)
  :config
  (evil-set-initial-state 'neotree-mode 'motion)
  (add-hook 'neo-after-create-hook 'doom|hide-mode-line)

  ;; Don't mess with neotree on wg-related window-config changes
  (advice-add 'doom/undo-window-change :around 'doom*save-neotree)
  (advice-add 'doom/redo-window-change :around 'doom*save-neotree)
  ;; A custom and simple theme for neotree
  (advice-add 'neo-buffer--insert-fold-symbol :override 'doom*neo-theme)
  ;; Shorter pwd in neotree
  (advice-add 'neo-buffer--insert-root-entry :filter-args 'doom*neotree-shorten-pwd)
  ;; Don't ask for confirmation when creating files
  (advice-add 'neotree-create-node :around 'doom*neotree-create-node)
  ;; Prevents messing up the neotree buffer on window changes
  (advice-add 'doom/evil-window-move :around 'doom*save-neotree)

  (defun doom*neotree-no-fringes ()
    (set-window-fringes neo-global--window 1 0))
  (advice-add 'neo-global--select-window :after 'doom*neotree-no-fringes)

  (add-hook 'neotree-mode-hook 'hl-line-mode)
  (add-hook 'neotree-mode-hook 'doom|neotree-init-keymap)
  (defun doom|neotree-init-keymap ()
    (map! :map evil-motion-state-local-map
          "ESC ESC" 'neotree-hide
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
          "R"   'neotree-change-root)))

;;
(use-package projectile
  :config
  (setq projectile-require-project-root nil
        projectile-enable-caching t
        projectile-cache-file (concat doom-temp-dir "/projectile.cache")
        projectile-known-projects-file (concat doom-temp-dir "/projectile.projects")
        projectile-indexing-method 'alien
        projectile-project-root-files doom-project-root-files
        projectile-file-exists-remote-cache-expire nil)

  ;; Don't cache ignored files!
  (defun doom*projectile-cache-current-file (orig-fun &rest args)
    (unless (--any (f-descendant-of? buffer-file-name it) (projectile-ignored-directories))
      (apply orig-fun args)))
  (advice-add 'projectile-cache-current-file :around 'doom*projectile-cache-current-file)

  (push "ido.last" projectile-globally-ignored-files)
  (push "assets"   projectile-globally-ignored-directories)
  (push ".cask"    projectile-globally-ignored-directories)

  (push ".elc"     projectile-globally-ignored-file-suffixes)
  (push doom-temp-dir projectile-globally-ignored-directories)

  (projectile-global-mode +1))

(provide 'core-project)
;;; core-project.el ends here
