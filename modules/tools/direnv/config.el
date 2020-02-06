;;; tools/direnv/config.el -*- lexical-binding: t; -*-

(defvar +direnv-keywords
  '("direnv_layout_dir" "PATH_add" "path_add" "log_status" "log_error" "has"
    "join_args" "expand_path" "dotenv" "user_rel_path" "find_up" "source_env"
    "watch_file" "source_up" "direnv_load" "MANPATH_add" "load_prefix" "layout"
    "use" "rvm" "use_nix" "use_guix")
  "TODO")


;;
;;; Packages

(use-package! direnv
  :hook (before-hack-local-variables  . direnv--maybe-update-environment)
  :hook (flycheck-before-syntax-check . direnv--maybe-update-environment)
  :hook (direnv-envrc-mode . +direnv-envrc-fontify-keywords-h)
  :config
  (add-to-list 'direnv-non-file-modes 'vterm-mode)

  (defun +direnv-envrc-fontify-keywords-h ()
    "Fontify special .envrc keywords; it's a good indication of whether or not
we've typed them correctly."
    (font-lock-add-keywords
     nil `((,(regexp-opt +direnv-keywords 'symbols)
            (0 font-lock-keyword-face)))))

  (defadvice! +direnv--fail-gracefully-a (&rest _)
    "Don't try to use direnv if the executable isn't present."
    :before-while #'direnv-update-directory-environment
    (or (executable-find "direnv")
        (ignore (doom-log "Couldn't find direnv executable"))))

  (direnv-mode +1))
