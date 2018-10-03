;;; feature/workspaces/config.el -*- lexical-binding: t; -*-

;; `persp-mode' gives me workspaces, a workspace-restricted `buffer-list', and
;; file-based session persistence. I used workgroups2 before this, but abandoned
;; it because it was unstable and slow; `persp-mode' is neither (and still
;; maintained).
;;
;; By default, sessions are autosaved, but not autoloaded. Use :ss or
;; `+workspace/save-session' to save, and :sl or `+workspace/load-session' to
;; load the last autosaved session. You can give sessions a custom name so they
;; can be loaded later.
;;
;; NOTE persp-mode requires `workgroups' for file persistence in Emacs 24.4.

(defvar +workspaces-main "main"
  "The name of the primary and initial workspace, which cannot be deleted.")

(defvar +workspaces-switch-project-function #'doom-project-find-file
  "The function to run after `projectile-switch-project' or
`counsel-projectile-switch-project'. This function must take one argument: the
new project directory.")

;; FIXME actually use this for wconf bookmark system
(defvar +workspaces-data-file "_workspaces"
  "The basename of the file to store single workspace perspectives. Will be
stored in `persp-save-dir'.")

(defvar +workspaces-on-switch-project-behavior 'non-empty
  "Controls the behavior of workspaces when switching to a new project.

Can be one of the following:

t           Always create a new workspace for the project
'non-empty  Only create a new workspace if the current one has no buffers
            associated with it.
nil         Never create a new workspace on project switch.")

;; If emacs is passed --restore, restore the last session on startup. This is
;; used by the `+workspace/restart-emacs-then-restore' command.
(defun +workspaces-restore-last-session (&rest _)
  (add-hook 'emacs-startup-hook #'+workspace/load-session :append))
(add-to-list 'command-switch-alist (cons "--restore" #'+workspaces-restore-last-session))


;;
;; Packages

(def-package! persp-mode
  :defer t
  :init
  (defun +workspaces|init ()
    ;; Remove default buffer predicate so persp-mode can put in its own
    (setq default-frame-alist
          (delq (assq 'buffer-predicate default-frame-alist)
                default-frame-alist))
    (add-hook 'after-make-frame-functions #'+workspaces|init-frame)
    (require 'persp-mode)
    (unless (daemonp)
      (+workspaces|init-frame (selected-frame))))

  (defun +workspaces|init-frame (frame)
    "Ensure a main workspace exists and is switched to, if FRAME isn't in any
workspace. Also ensures that the *Warnings* buffer will be visible in main.

Uses `+workspaces-main' to determine the name of the main workspace."
    (unless persp-mode
      (persp-mode +1))
    (unless noninteractive
      (let (persp-before-switch-functions persp-activated-functions)
        (with-selected-frame frame
          ;; The default perspective persp-mode creates (`persp-nil-name') is
          ;; special and doesn't represent a real persp object, so buffers can't
          ;; really be assigned to it, among other quirks. We create a *real*
          ;; main workspace to fill this role.
          (unless (persp-get-by-name +workspaces-main)
            (persp-add-new +workspaces-main))
          ;; Switch to it if we aren't auto-loading the last session
          (when (and (string= (safe-persp-name (get-current-persp)) persp-nil-name)
                     (= persp-auto-resume-time -1))
            (persp-frame-switch +workspaces-main frame)
            ;; We want to know where we are in every new daemon frame
            (when (daemonp)
              (run-at-time 0.1 nil #'+workspace/display))
            ;; Fix #319: the warnings buffer gets swallowed by creating
            ;; `+workspaces-main', so we display it manually, if it exists.
            (when-let* ((warnings (get-buffer "*Warnings*")))
              (save-excursion
                (display-buffer-in-side-window
                 warnings '((window-height . shrink-window-if-larger-than-buffer))))))))))

  (add-hook 'doom-post-init-hook #'+workspaces|init t)
  :config
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-nil-hidden t
        persp-auto-save-fname "autosave"
        persp-save-dir (concat doom-etc-dir "workspaces/")
        persp-set-last-persp-for-new-frames t
        persp-switch-to-added-buffer nil
        persp-remove-buffers-from-nil-persp-behaviour nil
        persp-auto-resume-time -1 ; Don't auto-load on startup
        persp-auto-save-opt (if noninteractive 0 1)) ; auto-save on kill

  (advice-add #'persp-asave-on-exit :around #'+workspaces*autosave-real-buffers)

  (add-hook 'doom-cleanup-hook #'+workspaces|cleanup-unassociated-buffers)

  ;; Ensure buffers we've opened/switched to are auto-added to the current
  ;; perspective
  (setq persp-add-buffer-on-find-file t
        persp-add-buffer-on-after-change-major-mode t)
  (add-hook 'persp-add-buffer-on-after-change-major-mode-filter-functions #'doom-unreal-buffer-p)

  (defun +workspaces|init-persp-mode ()
    (cond (persp-mode
           ;; `persp-kill-buffer-query-function' must be last
           (remove-hook 'kill-buffer-query-functions 'persp-kill-buffer-query-function)
           (add-hook 'kill-buffer-query-functions 'persp-kill-buffer-query-function t)
           ;; Restrict buffer list to workspace
           (advice-add #'doom-buffer-list :override #'+workspace-buffer-list))
          ((advice-remove #'doom-buffer-list #'+workspace-buffer-list))))
  (add-hook 'persp-mode-hook #'+workspaces|init-persp-mode)

  (defun +workspaces|leave-nil-perspective (&rest _)
    (when (string= (+workspace-current-name) persp-nil-name)
      (+workspace-switch (or (if (+workspace-p +workspace--last) +workspace--last)
                             (car (+workspace-list-names))
                             +workspaces-main)
                         'auto-create)))
  (add-hook 'persp-after-load-state-functions #'+workspaces|leave-nil-perspective)

  ;; Delete the current workspace if closing the last open window
  (define-key! persp-mode-map
    [remap delete-window] #'+workspace/close-window-or-workspace
    [remap evil-delete-window] #'+workspace/close-window-or-workspace)

  ;; per-frame workspaces
  (setq persp-init-frame-behaviour t
        persp-init-new-frame-behaviour-override nil
        persp-interactive-init-frame-behaviour-override #'+workspaces|associate-frame
        persp-emacsclient-init-frame-behaviour-override #'+workspaces|associate-frame)
  (add-hook 'delete-frame-functions #'+workspaces|delete-associated-workspace)

  ;; per-project workspaces, but reuse current workspace if empty
  (setq projectile-switch-project-action #'+workspaces|set-project-action
        counsel-projectile-switch-project-action
        '(1 ("o" +workspaces|switch-to-project "open project in new workspace")
            ("O" counsel-projectile-switch-project-action "jump to a project buffer or file")
            ("f" counsel-projectile-switch-project-action-find-file "jump to a project file")
            ("d" counsel-projectile-switch-project-action-find-dir "jump to a project directory")
            ("b" counsel-projectile-switch-project-action-switch-to-buffer "jump to a project buffer")
            ("m" counsel-projectile-switch-project-action-find-file-manually "find file manually from project root")
            ("w" counsel-projectile-switch-project-action-save-all-buffers "save all project buffers")
            ("k" counsel-projectile-switch-project-action-kill-buffers "kill all project buffers")
            ("r" counsel-projectile-switch-project-action-remove-known-project "remove project from known projects")
            ("c" counsel-projectile-switch-project-action-compile "run project compilation command")
            ("C" counsel-projectile-switch-project-action-configure "run project configure command")
            ("e" counsel-projectile-switch-project-action-edit-dir-locals "edit project dir-locals")
            ("v" counsel-projectile-switch-project-action-vc "open project in vc-dir / magit / monky")
            ("s" (lambda (project) (let ((projectile-switch-project-action (lambda () (call-interactively #'+ivy/project-search))))
                                     (counsel-projectile-switch-project-by-name project))) "search project")
            ("xs" counsel-projectile-switch-project-action-run-shell "invoke shell from project root")
            ("xe" counsel-projectile-switch-project-action-run-eshell "invoke eshell from project root")
            ("xt" counsel-projectile-switch-project-action-run-term "invoke term from project root")
            ("X" counsel-projectile-switch-project-action-org-capture "org-capture into project")))

  (add-hook 'projectile-after-switch-project-hook #'+workspaces|switch-to-project)

  ;; In some scenarios, persp-mode throws error when Emacs tries to die,
  ;; preventing its death and trapping us in Emacs.
  (defun +workspaces*ignore-errors-on-kill-emacs (orig-fn)
    (ignore-errors (funcall orig-fn)))
  (advice-add #'persp-kill-emacs-h :around #'+workspaces*ignore-errors-on-kill-emacs)

  ;;
  ;; eshell
  (persp-def-buffer-save/load
   :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
   :save-vars '(major-mode default-directory))
  ;; compile
  (persp-def-buffer-save/load
   :mode 'compilation-mode :tag-symbol 'def-compilation-buffer
   :save-vars
   '(major-mode default-directory compilation-directory compilation-environment compilation-arguments))
  ;; Restore indirect buffers
  (defvar +workspaces--indirect-buffers-to-restore nil)
  (persp-def-buffer-save/load
   :tag-symbol 'def-indirect-buffer
   :predicate #'buffer-base-buffer
   :save-function (lambda (buf tag vars)
                    (list tag (buffer-name buf) vars
                          (buffer-name (buffer-base-buffer))))
   :load-function (lambda (savelist &rest _rest)
                    (cl-destructuring-bind (buf-name _vars base-buf-name &rest _)
                        (cdr savelist)
                      (push (cons buf-name base-buf-name)
                            +workspaces--indirect-buffers-to-restore)
                      nil)))
  (defun +workspaces|reload-indirect-buffers (&rest _)
    (dolist (ibc +workspaces--indirect-buffers-to-restore)
      (let* ((nbn (car ibc))
             (bbn (cdr ibc))
             (bb (get-buffer bbn)))
        (when bb
          (when (get-buffer nbn)
            (setq nbn (generate-new-buffer-name nbn)))
          (make-indirect-buffer bb nbn t))))
    (setq +workspaces--indirect-buffers-to-restore nil))
  (add-hook 'persp-after-load-state-functions #'+workspaces|reload-indirect-buffers))

