;;; lisp/cli/install.el --- Doom Emacs install wizard -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load! "packages")


;;
;;; Variables

;; None yet!


;;
;;; Commands

(defcli! ((install i))
    ((aot?     ("--aot") "Enable ahead-of-time native-compilation (if available)")
     &flags
     (config?  ("--config" :yes)  "Create `$DOOMDIR' or dummy files therein?")
     (envfile? ("--env" :yes)     "(Re)generate an envvars file? (see `$ doom help env`)")
     (install? ("--install" :yes) "Auto-install packages?")
     (fonts?   ("--fonts" :yes)   "Install (or prompt to install) nerd-icons fonts?")
     (hooks?   ("--hooks" :yes)   "Deploy Doom's git hooks to itself?")
     &context context)
  "Installs and sets up Doom Emacs for the first time.

This command does the following:

  1. Creates `$DOOMDIR' at ~/.config/doom (if it or ~/.doom.d doesn't exist),
  2. Copies ~/.config/emacs/templates/init.example.el to `$DOOMDIR'/init.el (if
     it doesn't exist),
  3. Creates dummy files for `$DOOMDIR'/{config,packages}.el,
  4. Prompts you to generate an envvar file (same as `$ doom env`),
  5. Installs any dependencies of enabled modules (specified by `$DOOMDIR'/init.el),
  6. And prompts to install nerd-icons' fonts

This command is idempotent and safe to reuse.

Change `$DOOMDIR' with the `--doomdir' option, e.g.

  ```
  $ doom --doomdir /other/doom/config install
  ```"
  (print! (green "Installing Doom Emacs!\n"))
  (let ((default-directory doom-emacs-dir)
        (yes? (doom-cli-context-suppress-prompts-p context)))
    ;; Create `doom-user-dir'
    (if (eq config? :no)
        (print! (warn "Not copying private config template, as requested"))
      ;; Create DOOMDIR in ~/.config/doom if ~/.config/emacs exists.
      (when (and (not (file-directory-p doom-user-dir))
                 (not (getenv "DOOMDIR")))
        (let ((xdg-config-dir (or (getenv "XDG_CONFIG_HOME") "~/.config")))
          (when (file-in-directory-p doom-emacs-dir xdg-config-dir)
            (setq doom-user-dir (expand-file-name "doom/" xdg-config-dir)))))

      (if (file-directory-p doom-user-dir)
          (print! (item "Skipping %s (already exists)") (path doom-user-dir))
        (make-directory doom-user-dir 'parents)
        (print! (success "Created %s") (path doom-user-dir)))

      ;; Create init.el, config.el & packages.el
      (print-group!
        (mapc (lambda (file)
                (cl-destructuring-bind (filename . template) file
                  (setq filename (doom-path doom-user-dir filename))
                  (if (file-exists-p filename)
                      (print! (item "Skipping %s (already exists)...") (path filename))
                    (print! (item "Creating %s...") (path filename))
                    (with-temp-file filename (insert-file-contents template))
                    (print! (success "Done!")))))
              (let ((template-dir (doom-path doom-emacs-dir "templates")))
                `((,doom-module-init-file
                   . ,(file-name-with-extension (doom-path template-dir doom-module-init-file)
                                                ".example.el"))
                  (,doom-module-config-file
                   . ,(file-name-with-extension (doom-path template-dir doom-module-config-file)
                                                ".example.el"))
                  (,doom-module-packages-file
                   . ,(file-name-with-extension (doom-path template-dir doom-module-packages-file)
                                                ".example.el")))))))

    ;; In case no init.el (or cli.el) was present before the config was deployed
    (doom-load (doom-path doom-user-dir doom-module-init-file) t)
    (doom-load (doom-path doom-user-dir "cli.el") t)

    ;; Ask if user would like an envvar file generated
    (if (eq envfile? :no)
        (print! (warn "Not generating envvars file, as requested"))
      (if (file-exists-p doom-env-file)
          (print! (item "Envvar file already exists, skipping"))
        (when (or yes? (y-or-n-p "Generate an envvar file? (see `doom help env` for details)"))
          (call! '(env)))))

    (when aot?
      (after! straight
        (setq straight--native-comp-available t)))

    ;; Install Doom packages
    (if (eq install? :no)
        (print! (warn "Not installing plugins, as requested"))
      (print! (start "Installing plugins"))
      (print-group! (doom-packages-ensure)))

    (when (doom-profiles-bootloadable-p)
      (print! (start "Initializing profile bootstrapper..."))
      (call! '(profiles sync "--reload")))

    (print! (start "Synchronizing default profile..."))
    (print-group! (doom-profile-generate))

    (if (eq hooks? :no)
        (print! (warn "Not deploying commit-msg and pre-push git hooks, as requested"))
      (print! (start "Deploying commit-msg and pre-push git hooks"))
      (print-group!
       (condition-case e
           (call! `(ci deploy-hooks ,@(if yes? '("--force"))))
         ('user-error
          (print! (warn "%s") (error-message-string e))))))

    (when (file-exists-p "~/.emacs")
      (print! (warn "A ~/.emacs file was detected. This conflicts with Doom and should be deleted!")))

    (print! (success "Finished! Doom is ready to go!\n"))
    (with-temp-buffer
      (insert-file-contents (doom-path doom-emacs-dir "templates/QUICKSTART_INTRO"))
      (print! "%s" (buffer-string)))))

(provide 'doom-cli-install)
;;; install.el ends here
