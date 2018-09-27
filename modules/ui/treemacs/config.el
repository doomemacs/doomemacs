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
  :config
  ;; FIXME Remove this when general.el is in
  (set-evil-initial-state! 'treemacs-mode 'normal)
  (map! :map treemacs-mode-map
        :n "?"               #'treemacs-helpful-hydra
        :n [mouse-1]         #'treemacs-leftclick-action
        :n [double-mouse-1]  #'treemacs-doubleclick-action
        :n [tab]             #'treemacs-TAB-action
        :n [?\t]             #'treemacs-TAB-action
        :n [return]          #'treemacs-RET-action
        :n "RET"       #'treemacs-RET-action
        :n "r"         #'treemacs-refresh
        :n "d"         #'treemacs-delete
        :n "cf"        #'treemacs-create-file
        :n "cd"        #'treemacs-create-dir
        :n "R"         #'treemacs-rename
        :n "u"         #'treemacs-goto-parent-node
        :n "q"         #'bury-buffer
        :n "Q"         #'treemacs-kill-buffer
        :n "ov"        #'treemacs-visit-node-vertical-split
        :n "oh"        #'treemacs-visit-node-horizontal-split
        :n "oo"        #'treemacs-visit-node-no-split
        :n "oaa"       #'treemacs-visit-node-ace
        :n "oah"       #'treemacs-visit-node-ace-horizontal-split
        :n "oav"       #'treemacs-visit-node-ace-vertical-split
        :n "ox"        #'treemacs-visit-node-in-external-application
        :n "P"         #'treemacs-peek
        :n "n"         #'treemacs-next-line
        :n "p"         #'treemacs-previous-line
        :n "M-N"       #'treemacs-next-line-other-window
        :n "M-P"       #'treemacs-previous-line-other-window
        :n "<prior>"   #'treemacs-previous-page-other-window
        :n "<next>"    #'treemacs-next-page-other-window
        :n "M-n"       #'treemacs-next-neighbour
        :n "M-p"       #'treemacs-previous-neighbour
        :n "th"        #'treemacs-toggle-show-dotfiles
        :n "tw"        #'treemacs-toggle-fixed-width
        :n "tv"        #'treemacs-fringe-indicator-mode
        :n "tg"        #'treemacs-git-mode
        :n "tf"        #'treemacs-follow-mode
        :n "ta"        #'treemacs-filewatch-mode
        :n "w"         #'treemacs-set-width
        :n "yy"        #'treemacs-copy-path-at-point
        :n "yr"        #'treemacs-copy-project-root
        :n "s"         #'treemacs-resort
        :n "b"         #'treemacs-add-bookmark
        :n "C-p r"     #'treemacs-rename-project
        :n "C-p a"     #'treemacs-add-project-to-workspace
        :n "C-p d"     #'treemacs-remove-project-from-workspace
        :n "C-p c c"   #'treemacs-collapse-project
        :n "C-p c o"   #'treemacs-collapse-other-projects
        :n "C-p c a"   #'treemacs-collapse-all-projects
        :n "<backtab>" #'treemacs-collapse-all-projects
        :n "C-j"       #'treemacs-next-project
        :n "C-k"       #'treemacs-previous-project
        :n "h"         #'treemacs-root-up
        :n "l"         #'treemacs-root-down
        :n [escape] #'delete-window
        :n "j"   #'treemacs-next-line
        :n "k"   #'treemacs-previous-line
        :n "M-j" #'treemacs-next-neighbour
        :n "M-k" #'treemacs-previous-neighbour
        :n "M-J" #'treemacs-next-line-other-window
        :n "M-K" #'treemacs-previous-line-other-window
        :n "th"  #'treemacs-toggle-show-dotfiles
        :n "tw"  #'treemacs-toggle-fixed-width
        :n "tv"  #'treemacs-fringe-indicator-mode
        :n "tf"  #'treemacs-follow-mode
        :n "ta"  #'treemacs-filewatch-mode
        :n "tg"  #'treemacs-git-mode
        :n "w"   #'treemacs-set-width
        :n "b"   #'treemacs-add-bookmark
        :n "?"   #'treemacs-helpful-hydra
        :n "RET" #'treemacs-RET-action
        :n "yr"     #'treemacs-copy-project-root
        :n "yy"     #'treemacs-copy-path-at-point
        :n "gr"     #'treemacs-refresh
        :n [down-mouse-1] #'ignore
        :n "h"      #'treemacs-root-up
        :n "l"      #'treemacs-root-down))


(def-package! treemacs-projectile
  :after treemacs)
