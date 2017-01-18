;;; core-project.el --- tools for getting around your project

;; I want Emacs to be nominally aware of the projects. `projectile' provides
;; tools for digging through project files and exposing an API I can use to make
;; other plugins/features project-aware.
(package! projectile :demand t
  :init
  (setq projectile-cache-file (concat doom-temp-dir "/projectile.cache")
        projectile-completion-system 'ivy
        projectile-enable-caching t
        projectile-file-exists-remote-cache-expire nil
        projectile-globally-ignored-directories `(,doom-temp-dir ".sync")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-globally-ignored-files '(".DS_Store" "Icon")
        projectile-indexing-method 'alien
        projectile-known-projects-file (concat doom-temp-dir "/projectile.projects")
        projectile-project-root-files '(".git" ".hg" ".svn" ".project")
        projectile-require-project-root nil)

  :config
  (projectile-global-mode +1)

  (mapc (lambda (r) (push r projectile-other-file-alist))
        '(("less" "css")
          ("styl" "css")
          ("sass" "css")
          ("scss" "css")
          ("css" "scss" "sass" "less" "styl")
          ("jade" "html")
          ("pug" "html")
          ("html" "jade" "pug" "jsx" "tsx")))

  (defun doom-project-p (&optional strict-p)
    "Whether or not this buffer is currently in a project or not."
    (let ((projectile-require-project-root strict-p))
      (projectile-project-p)))

  (defun doom-project-root (&optional strict-p)
    "Get the path to the root of your project."
    (let ((projectile-require-project-root strict-p))
      (projectile-project-root)))

  (defun doom-project-has-files (files &optional root)
    "Return non-nil if FILES exist in the project root."
    (let ((root (or root (doom-project-root)))
          (files (if (listp files) files (list files)))
          (found-p (if files t)))
      (while (and found-p files)
        (let ((file (expand-file-name (pop files) root)))
          (setq found-p (if (string-suffix-p "/" file)
                            (file-directory-p file)
                          (file-exists-p file)))))
      found-p))

  (defun doom*projectile-cache-current-file (orig-fun &rest args)
    "Don't cache ignored files."
    (unless (--any (f-descendant-of? buffer-file-name it)
                   (projectile-ignored-directories))
      (apply orig-fun args)))
  (advice-add 'projectile-cache-current-file :around 'doom*projectile-cache-current-file))


;;
;; Autoloaded Packages
;;

;; A side-panel for browsing my project files. Inspired by vim's NERDTree.
(package! neotree
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
        neo-window-width 25
        neo-show-updir-line nil
        neo-theme 'nerd ; fallback
        neo-banner-message nil
        neo-show-hidden-files nil
        neo-hidden-regexp-list
        '(;; vcs folders
          "^\\.\\(git\\|hg\\|svn\\)$"
          ;; compiled files
          "\\.\\(pyc\\|o\\|elc\\|lock\\|css.map\\)$"
          ;; generated files, caches or local pkgs
          "^\\(node_modules\\|vendor\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
          ;; org-mode folders
          "^\\.\\(sync\\|export\\|attach\\)$"
          "~$"
          "^#.*#$"))

  :config
  (evil-set-initial-state 'neotree-mode 'motion)

  ;; Adding keybindings to `neotree-mode-map' wouldn't work for me (they get
  ;; overridden when the neotree buffer is spawned). So we bind them in a hook.
  (add-hook 'neo-after-create-hook 'doom|neotree-init-keymap)
  (defun doom|neotree-init-keymap (&rest _)
    (map! :Lm "\\\\"     'evil-window-prev
          :Lm "ESC ESC"  'neotree-hide
          :Lm "q"        'neotree-hide
          :Lm [return]   'neotree-enter
          :Lm "RET"      'neotree-enter
          :Lm "<return>" 'neotree-enter
          :Lm "J"        'neotree-select-next-sibling-node
          :Lm "K"        'neotree-select-previous-sibling-node
          :Lm "H"        'neotree-select-up-node
          :Lm "L"        'neotree-select-down-node
          :Lm "v"        'neotree-enter-vertical-split
          :Lm "s"        'neotree-enter-horizontal-split
          :Lm "c"        'neotree-create-node
          :Lm "d"        'neotree-delete-node
          :Lm "C-r"      'neotree-refresh
          :Lm "r"        'neotree-rename-node
          :Lm "R"        'neotree-change-root)))

(provide 'core-project)
;;; core-project.el ends here
