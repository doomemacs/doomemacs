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
    (&flags
     (config?  ("--config" :yes)  "Create `$DOOMDIR' or dummy files therein?")
     (envfile? ("--env" :yes)     "(Re)generate an envvars file? (see `$ doom help env`)")
     (install? ("--install" :yes) "Auto-install packages?")
     (fonts?   ("--fonts" :yes)   "Install (or prompt to install) all-the-icons fonts?")
     (hooks?   ("--hooks" :yes)   "Deploy Doom's git hooks to itself?")
     &context context)
  "Installs and sets up Doom Emacs for the first time.

This command does the following:

  1. Creates `$DOOMDIR' at ~/.doom.d,
  2. Copies ~/.emacs.d/templates/init.example.el to `$DOOMDIR'/init.el (if it
     doesn't exist),
  3. Creates dummy files for `$DOOMDIR'/{config,packages}.el,
  4. Prompts you to generate an envvar file (same as `$ doom env`),
  5. Installs any dependencies of enabled modules (specified by `$DOOMDIR'/init.el),
  6. And prompts to install all-the-icons' fonts

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
          (print! (item "Skipping %s (already exists)") (relpath doom-user-dir))
        (make-directory doom-user-dir 'parents)
        (print! (success "Created %s") (relpath doom-user-dir)))

      ;; Create init.el, config.el & packages.el
      (print-group!
        (mapc (lambda (file)
                (cl-destructuring-bind (filename . template) file
                  (if (file-exists-p! filename doom-user-dir)
                      (print! (item "Skipping %s (already exists)")
                              (path filename))
                    (print! (item "Creating %s%s") (relpath doom-user-dir) filename)
                    (with-temp-file (doom-path doom-user-dir filename)
                      (insert-file-contents template))
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

    ;; In case no init.el was present the first time it was loaded.
    (doom-load (doom-path doom-user-dir doom-module-init-file) t)

    ;; Ask if user would like an envvar file generated
    (if (eq envfile? :no)
        (print! (warn "Not generating envvars file, as requested"))
      (if (file-exists-p doom-env-file)
          (print! (item "Envvar file already exists, skipping"))
        (when (or yes? (y-or-n-p "Generate an envvar file? (see `doom help env` for details)"))
          (call! '(env)))))

    ;; Install Doom packages
    (if (eq install? :no)
        (print! (warn "Not installing plugins, as requested"))
      (print! "Installing plugins")
      (doom-packages-install))

    (print! "Regenerating autoloads files")
    (doom-profile-generate)

    (if (eq hooks? :no)
        (print! (warn "Not deploying commit-msg and pre-push git hooks, as requested"))
      (print! "Deploying commit-msg and pre-push git hooks")
      (print-group!
       (condition-case e
           (call! `(ci deploy-hooks ,@(if yes? '("--force"))))
         ('user-error
          (print! (warn "%s") (error-message-string e))))))

    (cond ((eq fonts? :no))
          (IS-WINDOWS
           (print! (warn "Doom cannot install all-the-icons' fonts on Windows!\n"))
           (print-group!
            (print!
             (concat "You'll have to do so manually:\n\n"
                     "  1. Launch Doom Emacs\n"
                     "  2. Execute 'M-x all-the-icons-install-fonts' to download the fonts\n"
                     "  3. Open the download location in windows explorer\n"
                     "  4. Open each font file to install them"))))
          ((or yes? (y-or-n-p "Download and install all-the-icon's fonts?"))
           (require 'all-the-icons)
           (let ((window-system (cond (IS-MAC 'ns)
                                      (IS-LINUX 'x))))
             (all-the-icons-install-fonts 'yes))))

    (when (file-exists-p "~/.emacs")
      (print! (warn "A ~/.emacs file was detected. This conflicts with Doom and should be deleted!")))

    (print! (success "\nFinished! Doom is ready to go!\n"))
    (with-temp-buffer
      (insert-file-contents (doom-path doom-emacs-dir "templates/QUICKSTART_INTRO"))
      (print! "%s" (buffer-string)))))

(provide 'doom-cli-install)
;;; install.el ends here
