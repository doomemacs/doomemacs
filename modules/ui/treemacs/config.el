;;; ui/treemacs/config.el -*- lexical-binding: t; -*-

(setq treemacs-no-png-images t
      treemacs-follow-after-init t
      treemacs-width 35
      treemacs-position 'left
      treemacs-is-never-other-window t
      treemacs-silent-refresh nil
      treemacs-indentation 2
      treemacs-sorting 'alphabetic-desc
      treemacs-show-hidden-files t
      treemacs-goto-tag-strategy 'refetch-index)


(after! treemacs-persistence
  (setq treemacs--persist-file (concat doom-cache-dir "treemacs-persist"))

  ;; Because `treemacs--persist-file' is defined with `defconst' and used
  ;; immediately afterwards, it cannot be easily customized before
  ;; `treemacs--restore' uses its value. So, we call it a second time so the
  ;; customization can take effect.
  (unless (featurep 'treemacs)
    (treemacs--restore)))


(after! treemacs
  (defvar +treemacs-use-git-mode
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t) 'extended)
      (`(t . _) 'simple))
    "Type of git integration for `treemacs-git-mode'.
There are 2 possible values:
1) simple, which highlights only files based on their git status, and is
   slightly faster
2) extended, which highlights both files and directories, but requires python")

  (defvar treemacs-collapse-dirs
    (if (executable-find "python3") 3 0))

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (when (memq +treemacs-use-git-mode '(simple extended))
    (treemacs-git-mode +treemacs-use-git-mode))

  (add-hook 'treemacs-mode-hook #'hide-mode-line-mode))


(def-package! treemacs-evil
  :when (featurep! :feature evil +everywhere)
  :after treemacs
  :config (define-key evil-treemacs-state-map [escape] #'delete-window))


(def-package! treemacs-projectile
  :after treemacs)
