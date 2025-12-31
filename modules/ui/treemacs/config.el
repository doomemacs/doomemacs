;;; ui/treemacs/config.el -*- lexical-binding: t; -*-

(defvar +treemacs-git-mode 'simple
  "Type of git integration for `treemacs-git-mode'.

There are 3 possible values:

  1) `simple', which highlights only files based on their git status, and is
     slightly faster,
  2) `extended', which highlights both files and directories, but requires
     python,
  3) `deferred', same as extended, but highlights asynchronously.

This must be set before `treemacs' has loaded.")


;;
;;; Packages

(use-package! treemacs
  :defer t
  :init
  (setq treemacs-follow-after-init t
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-asc
        treemacs-persist-file (file-name-concat doom-profile-cache-dir "treemacs-persist")
        treemacs-last-error-persist-file (file-name-concat doom-profile-cache-dir "treemacs-last-error-persist"))
  :config
  ;; Don't follow the cursor (it's more disruptive/jarring than helpful as a default)
  (treemacs-follow-mode -1)

  (set-popup-rule! "^ ?\\*Treemacs" :ignore t)
  (when +treemacs-git-mode
    ;; If they aren't supported, fall back to simpler methods
    (when (and (memq +treemacs-git-mode '(deferred extended))
               (not treemacs-python-executable)
               (not (executable-find "python3")))
      (setq +treemacs-git-mode 'simple))
    (treemacs-git-mode +treemacs-git-mode)
    (setq treemacs-collapse-dirs
          (if (memq +treemacs-git-mode '(extended deferred))
              3
            0))))


(use-package! treemacs-nerd-icons
  :defer t
  ;; HACK: Because `lsp-treemacs' mutates Treemacs' default theme, and
  ;;   `treemacs-nerd-icons' reads from it to populate its nerd-icons theme,
  ;;   load order is important to ensure they don't step on each other's toes.
  :init (with-eval-after-load (if (modulep! +lsp) 'lsp-treemacs 'treemacs)
          (require 'treemacs-nerd-icons))
  :config (treemacs-load-theme "nerd-icons"))


(use-package! treemacs-evil
  :when (modulep! :editor evil +everywhere)
  :defer t
  :init
  (after! treemacs (require 'treemacs-evil))
  (add-to-list 'doom-evil-state-alist '(?T . treemacs))
  :config
  (define-key! evil-treemacs-state-map
    [return] #'treemacs-RET-action
    [tab]    #'treemacs-TAB-action
    "TAB"    #'treemacs-TAB-action
    ;; REVIEW Fix #1875 to be consistent with C-w {v,s}, but this should really
    ;;        be considered upstream.
    "o v"    #'treemacs-visit-node-horizontal-split
    "o s"    #'treemacs-visit-node-vertical-split))


(use-package! treemacs-projectile
  :after treemacs)


(use-package! treemacs-magit
  :when (modulep! :tools magit)
  :after treemacs magit)


(use-package! treemacs-persp
  :when (modulep! :ui workspaces)
  :after treemacs
  :config (treemacs-set-scope-type 'Perspectives))


(use-package! lsp-treemacs
  :when (modulep! +lsp)
  :after treemacs)
