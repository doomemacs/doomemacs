;;; ui/treemacs/config.el -*- lexical-binding: t; -*-

(setq treemacs-follow-after-init t
      treemacs-width 35
      treemacs-position 'left
      treemacs-is-never-other-window t
      treemacs-silent-refresh nil
      treemacs-indentation 2
      treemacs-sorting 'alphabetic-desc
      treemacs-show-hidden-files t
      treemacs-goto-tag-strategy 'refetch-index
      treemacs-display-in-side-window t
      treemacs-persist-file (concat doom-cache-dir "treemacs-persist"))


(after! treemacs
  (set-popup-rule! "^ \\*Treemacs"
    :side treemacs-position
    :size treemacs-width
    :quit nil
    :ttl 0)

  (defvar +treemacs-use-git-mode
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t) 'extended)
      (`(t)     'simple))
    "Type of git integration for `treemacs-git-mode'.
There are 2 possible values:
1) simple, which highlights only files based on their git status, and is
   slightly faster
2) extended, which highlights both files and directories, but requires python")

  (defvar treemacs-collapse-dirs
    (if (executable-find "python3") 3 0))

  (defun +treemacs|improve-hl-line-contrast ()
    "`hl-line' doesn't stand out enough in some themes."
    (face-remap-add-relative 'hl-line 'region))
  (add-hook 'treemacs-mode-hook #'+treemacs|improve-hl-line-contrast)

  (treemacs-follow-mode -1)
  (treemacs-filewatch-mode t)
  (when (memq +treemacs-use-git-mode '(simple extended))
    (treemacs-git-mode +treemacs-use-git-mode)))


(def-package! treemacs-evil
  :when (featurep! :feature evil +everywhere)
  :after treemacs
  :config (define-key evil-treemacs-state-map [escape] #'delete-window))


(def-package! treemacs-projectile
  :after treemacs)
