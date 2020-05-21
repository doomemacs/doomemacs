;;; tools/magit/config.el -*- lexical-binding: t; -*-

(use-package! magit
  :commands magit-file-delete
  :defer-incrementally (dash f s with-editor git-commit package eieio lv transient)
  :init
  (setq magit-auto-revert-mode nil)  ; we do this ourselves further down
  ;; Must be set early to prevent ~/.emacs.d/transient from being created
  (setq transient-levels-file  (concat doom-etc-dir "transient/levels")
        transient-values-file  (concat doom-etc-dir "transient/values")
        transient-history-file (concat doom-etc-dir "transient/history"))
  :config
  (setq transient-default-level 5
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        magit-diff-refine-hunk t ; show granular diffs in selected hunk
        ;; Don't autosave repo buffers. This is too magical, and saving can
        ;; trigger a bunch of unwanted side-effects, like save hooks and
        ;; formatters. Trust us to know what we're doing.
        magit-save-repository-buffers nil
        ;; Magit runs git *a lot*. Having to scan your PATH so many times can
        ;; add up with each invokation, especially on Catalina (macOS) or
        ;; Windows, so we resolve it once.
        magit-git-executable (executable-find magit-git-executable))

  (defadvice! +magit-revert-repo-buffers-deferred-a (&rest _)
    :after '(magit-checkout magit-branch-and-checkout)
    ;; Since the project likely now contains new files, best we undo the
    ;; projectile cache so it can be regenerated later.
    (projectile-invalidate-cache nil)
    ;; Use a more efficient strategy to auto-revert buffers whose git state has
    ;; changed: refresh the visible buffers immediately...
    (+magit-mark-stale-buffers-h))
  ;; ...then refresh the rest only when we switch to them, not all at once.
  (add-hook 'doom-switch-buffer-hook #'+magit-revert-buffer-maybe-h)

  ;; The default location for git-credential-cache is in
  ;; ~/.cache/git/credential. However, if ~/.git-credential-cache/ exists, then
  ;; it is used instead. Magit seems to be hardcoded to use the latter, so here
  ;; we override it to have more correct behavior.
  (unless (file-exists-p "~/.git-credential-cache/")
    (setq magit-credential-cache-daemon-socket
          (doom-glob (or (getenv "XDG_CACHE_HOME")
                         "~/.cache/")
                     "git/credential/socket")))

  ;; Prevent scrolling when manipulating magit-status hunks. Otherwise you must
  ;; reorient yourself every time you stage/unstage/discard/etc a hunk.
  ;; Especially so on larger projects."
  (defvar +magit--pos nil)
  (add-hook! 'magit-pre-refresh-hook
    (defun +magit--set-window-state-h ()
      (setq-local +magit--pos (list (current-buffer) (point) (window-start)))))
  (add-hook! 'magit-post-refresh-hook
    (defun +magit--restore-window-state-h ()
      (when (and +magit--pos (eq (current-buffer) (car +magit--pos)))
        (goto-char (cadr +magit--pos))
        (set-window-start nil (caddr +magit--pos) t)
        (kill-local-variable '+magit--pos))))

  ;; Magit uses `magit-display-buffer-traditional' to display windows, by
  ;; default, which is a little primitive. `+magit-display-buffer' marries
  ;; `magit-display-buffer-fullcolumn-most-v1' with
  ;; `magit-display-buffer-same-window-except-diff-v1', except:
  ;;
  ;; 1. Magit sub-buffers (like `magit-log') that aren't spawned from a status
  ;;    screen are opened as popups.
  ;; 2. The status screen isn't buried when viewing diffs or logs from the
  ;;    status screen.
  (setq transient-display-buffer-action '(display-buffer-below-selected)
        magit-display-buffer-function #'+magit-display-buffer-fn)
  (set-popup-rule! "^\\(?:\\*magit\\|magit:\\| \\*transient\\*\\)" :ignore t)
  (add-hook 'magit-popup-mode-hook #'hide-mode-line-mode)

  ;; Add additional switches that seem common enough
  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))

  ;; so magit buffers can be switched to (except for process buffers)
  (add-hook! 'doom-real-buffer-functions
    (defun +magit-buffer-p (buf)
      (with-current-buffer buf
        (and (derived-mode-p 'magit-mode)
             (not (eq major-mode 'magit-process-mode))))))

  ;; Clean up after magit by killing leftover magit buffers and reverting
  ;; affected buffers (or at least marking them as need-to-be-reverted).
  (define-key magit-status-mode-map [remap magit-mode-bury-buffer] #'+magit/quit)

  ;; Close transient with ESC
  (define-key transient-map [escape] #'transient-quit-one))


(use-package! forge
  :when (featurep! +forge)
  ;; We defer loading even further because forge's dependencies will try to
  ;; compile emacsql, which is a slow and blocking operation.
  :after-call magit-status
  :commands forge-create-pullreq forge-create-issue
  :preface
  (setq forge-database-file (concat doom-etc-dir "forge/forge-database.sqlite"))
  :config
  ;; All forge list modes are derived from `forge-topic-list-mode'
  (map! :map forge-topic-list-mode-map :n "q" #'kill-current-buffer)
  (set-popup-rule! "^\\*?[0-9]+:\\(?:new-\\|[0-9]+$\\)" :size 0.45 :modeline t :ttl 0 :quit nil)
  (set-popup-rule! "^\\*\\(?:[^/]+/[^ ]+ #[0-9]+\\*$\\|Issues\\|Pull-Requests\\|forge\\)" :ignore t)

  (defadvice! +magit--forge-get-repository-lazily-a (&rest _)
    "Make `forge-get-repository' return nil if the binary isn't built yet.
This prevents emacsql getting compiled, which appears to come out of the blue
and blocks Emacs for a short while."
    :before-while #'forge-get-repository
    (file-executable-p emacsql-sqlite-executable))

  (defadvice! +magit--forge-build-binary-lazily-a (&rest _)
    "Make `forge-dispatch' only build emacsql if necessary.
Annoyingly, the binary gets built as soon as Forge is loaded. Since we've
disabled that in `+magit--forge-get-repository-lazily-a', we must manually
ensure it is built when we actually use Forge."
    :before #'forge-dispatch
    (unless (file-executable-p emacsql-sqlite-executable)
      (emacsql-sqlite-compile 2)
      (if (not (file-executable-p emacsql-sqlite-executable))
          (message (concat "Failed to build emacsql; forge may not work correctly.\n"
                           "See *Compile-Log* buffer for details"))
        ;; HACK Due to changes upstream, forge doesn't initialize completely if
        ;;      it doesn't find `emacsql-sqlite-executable', so we have to do it
        ;;      manually after installing it.
        (setq forge--sqlite-available-p t)
        (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-pullreqs nil t)
        (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-issues   nil t)
        (after! forge-topic
          (dolist (hook forge-bug-reference-hooks)
            (add-hook hook #'forge-bug-reference-setup)))))))


(use-package! github-review
  :after magit
  :config
  (transient-append-suffix 'magit-merge "i"
    '("y" "Review pull request" +magit/start-github-review))
  (after! forge
    (transient-append-suffix 'forge-dispatch "c u"
      '("c r" "Review pull request" +magit/start-github-review))))


(use-package! magit-todos
  :after magit
  :config
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?") ; make colon optional
  (define-key magit-todos-section-map "j" nil)
  ;; Warns that jT isn't bound. Well, yeah, you don't need to tell me, that was
  ;; on purpose ya goose.
  (advice-add #'magit-todos-mode :around #'doom-shut-up-a))


(use-package! magit-gitflow
  :hook (magit-mode . turn-on-magit-gitflow))


(use-package! evil-magit
  :when (featurep! :editor evil +everywhere)
  :after magit
  :init
  (setq evil-magit-state 'normal
        evil-magit-use-z-for-folds t)
  :config
  (undefine-key! magit-mode-map
    ;; Replaced by z1, z2, z3, etc
    "M-1" "M-2" "M-3" "M-4"
    "1" "2" "3" "4"
    "0") ; moved to g=
  (evil-define-key* 'normal magit-status-mode-map [escape] nil) ; q is enough
  (evil-define-key* '(normal visual) magit-mode-map
    "%"  #'magit-gitflow-popup
    "zz" #'evil-scroll-line-to-center
    "g=" #'magit-diff-default-context)
  (define-key! 'normal
    (magit-status-mode-map
     magit-stash-mode-map
     magit-revision-mode-map
     magit-diff-mode-map)
    [tab] #'magit-section-toggle)
  (after! git-rebase
    (dolist (key '(("M-k" . "gk") ("M-j" . "gj")))
      (when-let (desc (assoc (car key) evil-magit-rebase-commands-w-descriptions))
        (setcar desc (cdr key))))
    (evil-define-key* evil-magit-state git-rebase-mode-map
      "gj" #'git-rebase-move-line-down
      "gk" #'git-rebase-move-line-up)))
