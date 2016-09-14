;;; core-project.el --- all your (basic) project navigational needs

(setq ;; Always copy/delete recursively
      dired-recursive-copies  'always
      dired-recursive-deletes 'top
      ;; Auto refresh dired, but be quiet about it
      global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

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

(use-package dired-k
  :after dired
  :config
  (setq dired-k-style 'git)
  (add-hook 'dired-initial-position-hook 'dired-k)
  (add-hook 'dired-after-readin-hook #'dired-k-no-revert))

;; Automatically create missing directories when creating new files
(defun doom|create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))
(push 'doom|create-non-existent-directory find-file-not-found-functions)

(use-package neotree
  :commands (neotree-show
             neotree-hide
             neotree-toggle
             neotree-dir
             neotree-find
             neo-global--with-buffer
             neo-global--window-exists-p)
  :init
  (setq neo-create-file-auto-open t
        neo-auto-indent-point nil
        neo-mode-line-type 'none
        neo-persist-show nil
        neo-window-width 24
        neo-show-updir-line nil
        neo-theme 'nerd ; fallback
        neo-banner-message nil)
  :config
  (evil-set-initial-state 'neotree-mode 'motion)
  (add-hook 'neo-after-create-hook 'doom-hide-mode-line-mode)

  ;; Don't ask for confirmation when creating files
  (advice-add 'neotree-create-node :around 'doom*neotree-create-node)
  ;; Prevents messing up the neotree buffer on window changes
  (advice-add 'doom/evil-window-move :around 'doom*save-neotree)

  (map! :map neotree-mode-map
        :m "\\\\" 'evil-window-prev
        "ESC ESC" 'neotree-hide
        "q"       'neotree-hide
        [return]  'neotree-enter
        "RET"     'neotree-enter
        :m "J"    'neotree-select-next-sibling-node
        :m "K"    'neotree-select-previous-sibling-node
        :m "H"    'neotree-select-up-node
        :m "L"    'neotree-select-down-node
        "v"       'neotree-enter-vertical-split
        "s"       'neotree-enter-horizontal-split
        "c"       'neotree-create-node
        "d"       'neotree-delete-node
        "g"       'neotree-refresh
        "r"       'neotree-rename-node
        "R"       'neotree-change-root))

(use-package projectile
  :config
  (setq projectile-require-project-root nil
        projectile-enable-caching t
        projectile-cache-file (concat doom-temp-dir "/projectile.cache")
        projectile-known-projects-file (concat doom-temp-dir "/projectile.projects")
        projectile-indexing-method 'alien
        projectile-file-exists-remote-cache-expire nil
        projectile-project-root-files '(".git" ".hg" ".svn" ".project"))

  ;; Don't cache ignored files!
  (defun doom*projectile-cache-current-file (orig-fun &rest args)
    (unless (--any (f-descendant-of? buffer-file-name it) (projectile-ignored-directories))
      (apply orig-fun args)))
  (advice-add 'projectile-cache-current-file :around 'doom*projectile-cache-current-file)

  (push doom-temp-dir projectile-globally-ignored-directories)
  (push "assets"   projectile-globally-ignored-directories)
  (push ".cask"    projectile-globally-ignored-directories)
  (push ".sync"    projectile-globally-ignored-directories)
  (push ".elc"     projectile-globally-ignored-file-suffixes)
  (push ".project" projectile-globally-ignored-file-suffixes)
  (push "Icon"   projectile-globally-ignored-files)

  (projectile-global-mode +1))

(provide 'core-project)
;;; core-project.el ends here
