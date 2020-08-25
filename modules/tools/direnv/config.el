;;; tools/direnv/config.el -*- lexical-binding: t; -*-

(defvar +direnv-keywords
  '("direnv_layout_dir" "PATH_add" "path_add" "log_status" "log_error" "has"
    "join_args" "expand_path" "dotenv" "user_rel_path" "find_up" "source_env"
    "watch_file" "source_up" "direnv_load" "MANPATH_add" "load_prefix" "layout"
    "use" "rvm" "use_nix" "use_guix")
  "A list of direnv keywords, which are fontified when in `+direnv-rc-mode'.")


;;
;;; Packages

(use-package! envrc
  :when (executable-find "direnv")
  :after-call doom-first-file-hook
  :mode ("\\.envrc\\'" . +direnv-rc-mode)
  :config
  (add-to-list 'doom-debug-variables 'envrc-debug)

  ;; I'm avoiding `global-envrc-mode' intentionally, because it has the
  ;; potential to run too late in the mode startup process (and after, say,
  ;; server hooks that may rely on that local direnv environment).
  (add-hook! 'change-major-mode-after-body-hook
    (defun +direnv-init-h ()
      (unless (or envrc-mode
                  (minibufferp)
                  (file-remote-p default-directory))
        (envrc-mode 1))))

  (define-derived-mode +direnv-rc-mode sh-mode "envrc"
    "Major mode for .envrc files."
    ;; Fontify .envrc keywords; it's a good indication of whether or not we've
    ;; typed them correctly, and that we're in the correct major mode.
    (font-lock-add-keywords
     nil `((,(regexp-opt +direnv-keywords 'symbols)
            (0 font-lock-keyword-face)))))

  (defadvice! +direnv--fail-gracefully-a (&rest _)
    "Don't try to use direnv if the executable isn't present."
    :before-while #'envrc-mode
    (or (executable-find "direnv")
        (ignore (doom-log "Couldn't find direnv executable"))))

  ;; HACK envrc-mode only affects the current buffer's environment, which is
  ;;      generally what we want, except when we're running babel blocks in
  ;;      org-mode, because there may be state or envvars those blocks need to
  ;;      read. In order to perpetuate the org buffer's environment into the
  ;;      execution of the babel block we need to temporarily change the global
  ;;      environment. Let's hope it runs quickly enough that its effects aren't
  ;;      felt in other buffers in the meantime!
  (defvar +direnv--old-environment nil)
  (defadvice! +direnv-persist-environment-a (orig-fn &rest args)
    :around #'org-babel-execute-src-block
    (if +direnv--old-environment
        (apply orig-fn args)
      (setq-default +direnv--old-environment
                    (cons (default-value 'process-environment)
                          (default-value 'exec-path))
                    exec-path exec-path
                    process-environment process-environment)
      (unwind-protect (apply orig-fn args)
        (setq-default process-environment (car +direnv--old-environment)
                      exec-path (cdr +direnv--old-environment)
                      +direnv--old-environment nil)))))
