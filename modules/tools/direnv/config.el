;;; tools/direnv/config.el -*- lexical-binding: t; -*-

(defvar +direnv--keywords
  '("direnv_layout_dir" "PATH_add" "path_add" "log_status" "log_error" "has"
    "join_args" "expand_path" "dotenv" "user_rel_path" "find_up" "source_env"
    "watch_file" "source_up" "direnv_load" "MANPATH_add" "load_prefix" "layout"
    "use" "rvm" "use_nix" "use_guix")
  "TODO")

(def-package! direnv
  :after-call (after-find-file dired-initial-position-hook)
  :config
  (add-hook 'direnv-mode-hook
    (defun +direnv-init-h ()
      "Instead of checking for direnv on `post-command-hook', check on
buffer/window/frame switch, which is less expensive."
      (direnv--disable)
      (when direnv-mode
        (add-hook 'doom-switch-buffer-hook #'direnv--maybe-update-environment)
        (add-hook 'doom-switch-window-hook #'direnv--maybe-update-environment)
        (add-hook 'doom-switch-frame-hook #'direnv--maybe-update-environment)
        (add-hook 'focus-in-hook #'direnv--maybe-update-environment))))

  ;; Fontify special .envrc keywords; it's a good indication of whether or not
  ;; we've typed them correctly.
  (add-hook 'direnv-envrc-mode-hook
    (defun +direnv-envrc-fontify-keywords-h ()
      (font-lock-add-keywords
       nil `((,(regexp-opt +direnv--keywords 'symbols)
              (0 font-lock-keyword-face))))))

  (def-advice! +direnv--update-a (&rest _)
    "Update direnv. Useful to advise functions that may run
environment-sensitive logic like `flycheck-default-executable-find'. This fixes
flycheck issues with direnv and on nix."
    :before #'flycheck-default-executable-find
    (direnv--maybe-update-environment))

  (def-advice! +direnv--fail-gracefully-a (orig-fn)
    "Don't try to update direnv if the executable isn't present."
    :around #'direnv--maybe-update-environment
    (if (executable-find "direnv")
        (funcall orig-fn)
      (doom-log "Couldn't find direnv executable")))

  (direnv-mode +1))
