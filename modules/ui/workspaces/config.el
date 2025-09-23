;;; ui/workspaces/config.el -*- lexical-binding: t; -*-

(defvar +workspaces-dir (file-name-concat doom-profile-data-dir "workspaces/")
  "The directory where workspace session files are stored.")

(defvar +workspaces--parameters (make-hash-table :test 'equal))


;;
;;; Packages

(use-package! tabspaces
  :hook (doom-init-ui . tabspaces-mode)
  :init
  (setq tabspaces-session-file (file-name-concat +workspaces-dir "session.el"))
  (if (modulep! +tabs)
      (setq tab-bar-show 1)  ; hide if only one workspace
    (setq tabspaces-echo-area-enable t))

  :config
  (setq tabspaces-session t
        tabspaces-session-auto-restore (modulep! +auto)
        tabspaces-use-filtered-buffers-as-default t
        tabspaces-default-tab "Main"
        tabspaces-remove-to-default nil
        tabspaces-fully-resolve-paths t)

  ;; Centralize project sessions rather than pollute projects with more dotfiles
  ;; (the default behavior).
  (setq tabspaces-session-project-session-store
        (lambda (proot)
          (doom-path +workspaces-dir "projects"
                     (format "%s.%s.session.el"
                             (doom-project-name proot)
                             (sha1 (file-truename proot))))))

  ;; Delete the current workspace if closing the last open window
  (define-key! tabspaces-mode-map
    [remap delete-window] #'+workspaces/close-window-or-workspace
    [remap evil-window-delete] #'+workspaces/close-window-or-workspace)

  ;; Per-workspace `winner-mode' history
  (add-to-list 'window-persistent-parameters '(winner-ring . t))


  ;; HACK: Emacs tabs don't offer an easy way to associate data with a tab,
  ;; since tab names aren't unique, so these advice is dedicated to providing
  ;; that:
  (autoload 'uuidgen-1 "uuidgen")
  (defadvice! +workspaces--embed-id-a (tab)
    :filter-return #'tab-bar--tab
    :filter-return #'tab-bar--current-tab-make
    (unless (alist-get 'id tab)
      (setq tab (append tab `((id . ,(uuidgen-1))))))
    tab)
  (add-to-list 'window-persistent-parameters '(id . writable))

  (add-hook 'tab-bar-tab-post-open-functions #'+workspaces-init-parameters-h)
  (add-hook 'tab-bar-tab-pre-close-functions #'+workspaces-cleanup-parameters-h)


  ;; HACK: Tabspaces is hardcoded to equate the root of a version controlled
  ;;   project as the only kind of project root, and lacks integration with
  ;;   project.el or projectile. This fixes that.
  ;; REVIEW: PR this upstream!
  (defadvice! +workspaces--project-root-a (fn &rest args)
    :around #'tabspaces-save-current-project-session
    :around #'tabspaces--get-project-session-file
    (letf! ((#'vc-root-dir #'doom-project-root))
      (apply fn args)))
  (defadvice! +workspaces--project-name-a (fn &rest args)
    :around #'tabspaces--project-name
    (or (doom-project-root)
        (apply fn args)))

  ;; HACK: Fix tabspaces-session-file living in a directory that doesn't exist.
  ;; REVIEW: PR this upstream!
  (defadvice! +workspaces--mkdir-before-save-a (&rest _)
    :before #'tabspaces-save-session
    (make-directory (file-name-directory tabspaces-session-file) t))

  ;; HACK: `tabspaces-open-or-create-project-and-workspace' has *so* much
  ;;   hardcoded and opinionated behavior, so I replace it with a version that
  ;;   cooperates with projectile/project, the dashboard, our custom
  ;;   `tabspaces-session-project-session-store', and fixes its bugs.
  ;; REVIEW: Issues should be raised upstream!
  (advice-add #'tabspaces-open-or-create-project-and-workspace
              :override #'+workspaces/open-in-project))
