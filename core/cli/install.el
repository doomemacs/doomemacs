;;; core/cli/install.el -*- lexical-binding: t; -*-

(defcli! (install i)
    ((noconfig-p  ["--no-config"]  "Don't create DOOMDIR or dummy files therein")
     (noenv-p     ["--no-env"]     "Don't generate an envvars file (see 'doom help env')")
     (noinstall-p ["--no-install"] "Don't auto-install packages")
     (nofonts-p   ["--no-fonts"]   "Don't install (or prompt to install) all-the-icons fonts")
     &rest _args)
  "Installs and sets up Doom Emacs for the first time.

This command does the following:

  1. Creates DOOMDIR at ~/.doom.d,
  2. Copies ~/.emacs.d/init.example.el to $DOOMDIR/init.el (if it doesn't exist),
  3. Creates dummy files for $DOOMDIR/{config,packages}.el,
  4. Prompts you to generate an envvar file (same as 'doom env'),
  5. Installs any dependencies of enabled modules (specified by $DOOMDIR/init.el),
  6. And prompts to install all-the-icons' fonts

This command is idempotent and safe to reuse.

The location of DOOMDIR can be changed with the -p option, or by setting the
DOOMDIR environment variable. e.g.

  doom -p ~/.config/doom install
  DOOMDIR=~/.config/doom doom install"
  (print! (green "Installing Doom Emacs!\n"))
  (let ((default-directory (doom-path "~")))
    ;; Create `doom-private-dir'
    (if noconfig-p
        (print! (warn "Not copying private config template, as requested"))
      (print! (start "Creating %s") (relpath doom-private-dir))
      (make-directory doom-private-dir 'parents)
      (print-group!
       (print! (success "Created %s") (relpath doom-private-dir)))

      ;; Create init.el, config.el & packages.el
      (mapc (lambda (file)
              (cl-destructuring-bind (filename . fn) file
                (if (file-exists-p! filename doom-private-dir)
                    (print! (warn "%s already exists, skipping") filename)
                  (print! (info "Creating %s%s") (relpath doom-private-dir) filename)
                  (with-temp-file (doom-path doom-private-dir filename)
                    (funcall fn))
                  (print! (success "Done!")))))
            '(("init.el" .
               (lambda ()
                 (insert-file-contents
                  (doom-path doom-emacs-dir "init.example.el"))))
              ("config.el" .
               (lambda ()
                 (insert-file-contents
                  (doom-path doom-core-dir "templates/config.example.el"))))
              ("packages.el" .
               (lambda ()
                 (insert-file-contents
                  (doom-path doom-core-dir "templates/packages.example.el")))))))

    ;; In case no init.el was present the first time `doom-initialize-modules' was
    ;; called in core.el (e.g. on first install)
    (ignore-errors
      (load! "init" doom-private-dir t))
    (when doom-modules
      (maphash (lambda (key plist)
                 (let ((doom--current-module key)
                       (doom--current-flags (plist-get plist :flags)))
                   (ignore-errors (load! "init" (plist-get plist :path) t))))
               doom-modules))

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
    (doom-cli-reload-autoloads)

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
      (insert-file-contents (doom-glob doom-core-dir "templates/QUICKSTART_INTRO"))
      (print! "%s" (buffer-string)))))
