;;; tools/magit/config.el -*- lexical-binding: t; -*-

(defvar +magit-default-clone-url "https://github.com/%s/%s"
  "The default location for `+magit/clone' to clone relative URLs from.
It is passed a user and repository name.")


;;
;; Packages

(def-package! magit
  :commands magit-file-delete
  :defer-incrementally (dash f s with-editor git-commit package eieio lv transient)
  :init
  (setq magit-auto-revert-mode nil)  ; we do this ourselves
  ;; Must be set early to prevent ~/.emacs.d/transient from being created
  (setq transient-levels-file  (concat doom-etc-dir "transient/levels")
        transient-values-file  (concat doom-etc-dir "transient/values")
        transient-history-file (concat doom-etc-dir "transient/history"))
  :config
  (setq transient-default-level 5
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        magit-diff-refine-hunk t) ; show granular diffs in selected hunk

  ;; Magit uses `magit-display-buffer-traditional' to display windows, by
  ;; default, which is a little primitive. `+magit-display-buffer' marries
  ;; `magit-display-buffer-fullcolumn-most-v1' with
  ;; `magit-display-buffer-same-window-except-diff-v1', except:
  ;;
  ;; 1. Magit sub-buffers (like `magit-log') that aren't spawned from a status
  ;;    screen are opened as popups.
  ;; 2. The status screen isn't buried when viewing diffs or logs from the
  ;;    status screen.
  (setq magit-display-buffer-function #'+magit-display-buffer)
  (set-popup-rule! "^\\(?:\\*magit\\|magit:\\)" :ignore t)

  ;; Add --tags switch
  (transient-append-suffix 'magit-fetch
    "-p" '("-t" "Fetch all tags" ("-t" "--tags")))

  ;; so magit buffers can be switched to (except for process buffers)
  (defun +magit-buffer-p (buf)
    (with-current-buffer buf
      (and (derived-mode-p 'magit-mode)
           (not (eq major-mode 'magit-process-mode)))))
  (add-to-list 'doom-real-buffer-functions #'+magit-buffer-p nil #'eq)

  ;; properly kill leftover magit buffers on quit
  (define-key magit-status-mode-map [remap magit-mode-bury-buffer] #'+magit/quit)

  ;; Close transient with ESC
  (define-key transient-map [escape] #'transient-quit-one))


(def-package! forge
  ;; We defer loading even further because forge's dependencies will try to
  ;; compile emacsql, which is a slow and blocking operation.
  :after-call magit-status
  :init
  (setq forge-database-file (concat doom-etc-dir "forge/forge-database.sqlite"))
  :config
  ;; All forge list modes are derived from `forge-topic-list-mode'
  (map! :map forge-topic-list-mode-map :n "q" #'kill-current-buffer)
  (set-popup-rule! "^\\*?[0-9]+:\\(?:new-\\|[0-9]+$\\)" :size 0.45 :modeline t :ttl 0 :quit nil)
  (set-popup-rule! "^\\*\\(?:[^/]+/[^ ]+ #[0-9]+\\*$\\|Issues\\|Pull-Requests\\|forge\\)" :ignore t))


(def-package! magit-todos
  :after magit
  :config
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?")
  (define-key magit-todos-section-map "j" nil)
  (advice-add #'magit-todos-mode :around #'doom*shut-up)
  (magit-todos-mode +1))


(def-package! magit-gitflow
  :hook (magit-mode . turn-on-magit-gitflow))


(def-package! evil-magit
  :when (featurep! :editor evil +everywhere)
  :after magit
  :init
  (setq evil-magit-state 'normal
        evil-magit-use-z-for-folds t)
  :config
  (unmap! magit-mode-map "M-1" "M-2" "M-3" "M-4") ; replaced by z1, z2, z3, etc
  (evil-define-key* 'normal magit-status-mode-map [escape] nil) ; q is enough
  (evil-define-key* '(normal visual) magit-mode-map
    "zz" #'evil-scroll-line-to-center
    "%"  #'magit-gitflow-popup)
  (define-key! 'normal
    (magit-status-mode-map
     magit-stash-mode-map
     magit-revision-mode-map
     magit-diff-mode-map)
    [tab] #'magit-section-toggle)
  (after! git-rebase
    (dolist (key '(("M-k" . "gk") ("M-j" . "gj")))
      (when-let* ((desc (assoc (car key) evil-magit-rebase-commands-w-descriptions)))
        (setcar desc (cdr key))))
    (evil-define-key* evil-magit-state git-rebase-mode-map
      "gj" #'git-rebase-move-line-down
      "gk" #'git-rebase-move-line-up)))
