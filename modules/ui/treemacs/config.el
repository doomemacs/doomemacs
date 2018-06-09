;;; ui/treemacs/config.el -*- lexical-binding: t; -*-

(defvar treemacs-use-git-mode
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t) 'extended)
    (`(t . _) 'simple))
  "Type of git integration for `treemacs-git-mode'.
There are 2 possible values:
1) simple, which highlights only files based on their git status, and is
   slightly faster
2) extended, which highlights both files and directories, but requires python")

(def-package! treemacs
  :commands (treemacs treemacs-find-file treemacs-bookmark)
  :config
  (setq treemacs-no-png-images t
        treemacs-follow-after-init t
        treemacs-width 35
        treemacs-position 'left
        treemacs-is-never-other-window t
        treemacs-silent-refresh nil
        treemacs-indentation 2
        treemacs-sorting 'alphabetic-desc
        treemacs-show-hidden-files t
        treemacs-goto-tag-strategy 'refetch-index
        treemacs-collapse-dirs (if (executable-find "python3") 3 0))
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (when (memq treemacs-use-git-mode '(simple extended))
    (treemacs-git-mode treemacs-use-git-mode)))

(add-hook! 'doom-post-init-hook
  (map! :leader
        :prefix "f"
        :desc "Open treemacs" :nv "t" #'treemacs
        :desc "Find file in treemacs" :nv "T" #'treemacs-find-file
        :desc "Go to bookmark" :nv "b" #'treemacs-bookmark))

(def-package! treemacs-evil
  :after (treemacs evil)
  :if (featurep! :feature evil))

(def-package! treemacs-projectile
  :after (treemacs projectile))
