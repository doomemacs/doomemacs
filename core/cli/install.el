;;; core/cli/install.el -*- lexical-binding: t; -*-

(defcli! (install i)
    ((noconfig-p  ["--no-config"]  "Don't create DOOMDIR or dummy files therein")
     (noenv-p     ["--no-env"]     "Don't generate an envvars file (see 'doom help env')")
     (noinstall-p ["--no-install"] "Don't auto-install packages")
     (nofonts-p   ["--no-fonts"]   "Don't install (or prompt to install) all-the-icons fonts"))
  "Installs and sets up Doom Emacs for the first time.

This command does the following:

  1. Creates DOOMDIR at ~/.doom.d,
  2. Copies ~/.emacs.d/init.example.el to $DOOMDIR/init.el (if it doesn't exist),
  3. Creates dummy files for $DOOMDIR/{config,packages}.el,
  4. Prompts you to generate an envvar file (same as 'doom env'),
  5. Installs any dependencies of enabled modules (specified by $DOOMDIR/init.el),
  6. And prompts to install all-the-icons' fonts

This command is idempotent and safe to reuse.

The location of DOOMDIR can be changed with the environment variable of the same
name. e.g.

  DOOMDIR=~/.config/doom doom install"
  (print! (green "Installing Doom Emacs!\n"))
  (let ((default-directory (doom-path "~")))
    ;; Create `doom-private-dir'
    (if noconfig-p
        (print! (warn "Not copying private config template, as requested"))
      ;; Create DOOMDIR in ~/.config/doom if ~/.config/emacs exists.
      (when (and (not (file-directory-p doom-private-dir))
                 (not (getenv "DOOMDIR")))
        (let ((xdg-config-dir (or (getenv "XDG_CONFIG_HOME") "~/.config")))
          (when (file-in-directory-p doom-emacs-dir xdg-config-dir)
            (setq doom-private-dir (expand-file-name "doom/" xdg-config-dir)))))
      (print! (start "Creating %s") (relpath doom-private-dir))
      (make-directory doom-private-dir 'parents)
      (print-group!
       (print! (success "Created %s") (relpath doom-private-dir)))

      ;; Create init.el, config.el & packages.el
      (mapc (lambda (file)
              (cl-destructuring-bind (filename . template) file
                (if (file-exists-p! filename doom-private-dir)
                    (print! (warn "%s already exists, skipping") filename)
                  (print! (info "Creating %s%s") (relpath doom-private-dir) filename)
                  (with-temp-file (doom-path doom-private-dir filename)
                    (insert-file-contents template))
                  (print! (success "Done!")))))
            `(("init.el" . ,(doom-path doom-emacs-dir "init.example.el"))
              ("config.el" . ,(doom-path doom-core-dir "templates/config.example.el"))
              ("packages.el" . ,(doom-path doom-core-dir "templates/packages.example.el")))))

    ;; In case no init.el was present the first time `doom-initialize-modules' was
    ;; called in core.el (e.g. on first install)
    (doom-initialize-modules 'force 'no-config)

    ;; Ask if user would like an envvar file generated
    (if noenv-p
        (print! (warn "Not generating envvars file, as requested"))
      (if (file-exists-p doom-env-file)
          (print! (info "Envvar file already exists, skipping"))
        (when (or doom-auto-accept
                  (y-or-n-p "Generate an envvar file? (see `doom help env` for details)"))
          (doom-cli-reload-env-file 'force-p))))

    ;; Install Doom packages
    (if noinstall-p
        (print! (warn "Not installing plugins, as requested"))
      (print! "Installing plugins")
      (doom-cli-packages-install))

    (print! "Regenerating autoloads files")
    (doom-autoloads-reload)

    (cond (nofonts-p)
          (IS-WINDOWS
           (print! (warn "Doom cannot install all-the-icons' fonts on Windows!\n"))
           (print-group!
            (print!
             (concat "You'll have to do so manually:\n\n"
                     "  1. Launch Doom Emacs\n"
                     "  2. Execute 'M-x all-the-icons-install-fonts' to download the fonts\n"
                     "  3. Open the download location in windows explorer\n"
                     "  4. Open each font file to install them"))))
          ((or doom-auto-accept
               (y-or-n-p "Download and install all-the-icon's fonts?"))
           (require 'all-the-icons)
           (let ((window-system (cond (IS-MAC 'ns)
                                      (IS-LINUX 'x))))
             (all-the-icons-install-fonts 'yes))))

    (when (file-exists-p "~/.emacs")
      (print! (warn "A ~/.emacs file was detected. This conflicts with Doom and should be deleted!")))

    (print! (success "\nFinished! Doom is ready to go!\n"))
    (with-temp-buffer
      (insert-file-contents (doom-path doom-core-dir "templates/QUICKSTART_INTRO"))
      (print! "%s" (buffer-string)))))
