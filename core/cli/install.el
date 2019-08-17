;;; core/cli/install.el -*- lexical-binding: t; -*-

(defcli! quickstart (&rest args)  ; DEPRECATED
  "This is a deprecated alias for 'doom install'.

See 'doom help install' instead."
  :hidden t
  (apply #'doom-cli-install args))

(defcli! (install i) (&rest args)
  "A wizard for installing Doom for the first time.

This command does the following:

  1. Creates DOOMDIR at ~/.doom.d,
  2. Copies ~/.emacs.d/init.example.el to DOOMDIR/init.el (if it doesn't exist),
  3. Creates dummy files for DOOMDIR/{config,packages}.el,
  4. Prompts you to generate an envvar file (via 'doom env refresh'),
  5. Installs any dependencies of enabled modules (specified by DOOMDIR/init.el),
  6. And prompts to install all-the-icons' fonts

This command is idempotent and safe to reuse.

The location of DOOMDIR can be changed with the -p option, or by setting the
DOOMDIR environment variable. e.g.

  doom -p ~/.config/doom install
  DOOMDIR=~/.config/doom doom install

install understands the following switches:

  --no-config    Don't create DOOMDIR or dummy files therein
  --no-install   Don't auto-install packages
  --no-env       Don't generate an envvars file (see `doom help env`)
  --no-fonts     Don't install (or prompt to install) all-the-icons fonts
  -y / --yes     Auto-accept any confirmation prompts"
  (print! (green "Installing Doom Emacs!\n"))
  (let ((default-directory (doom-path "~")))
    ;; Create `doom-private-dir'
    (if (member "--no-config" args)
        (print! (warn "Not copying private config template, as requested"))
      (print! "> Creating %s" (relpath doom-private-dir))
      (make-directory doom-private-dir 'parents)
      (print! (success "Created %s") (relpath doom-private-dir))

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
                 (insert-file-contents (doom-path doom-emacs-dir "init.example.el"))))
              ("config.el" .
               (lambda ()
                 (insert! ";;; %sconfig.el -*- lexical-binding: t; -*-\n\n"
                          ";; Place your private configuration here\n"
                          ((relpath doom-private-dir)))))
              ("packages.el" .
               (lambda ()
                 (insert! ";; -*- no-byte-compile: t; -*-\n;;; %spackages.el\n\n"
                          ";;; Examples:\n"
                          ";; (package! some-package)\n"
                          ";; (package! another-package :recipe (:host github :repo \"username/repo\"))\n"
                          ";; (package! builtin-package :disable t)\n"
                          ((relpath doom-private-dir))))))))

    ;; In case no init.el was present the first time `doom-initialize-modules' was
    ;; called in core.el (e.g. on first install)
    (doom-initialize-packages 'force-p)

    ;; Ask if Emacs.app should be patched
    (if (member "--no-env" args)
        (print! (warn "- Not generating envvars file, as requested"))
      (when (or doom-auto-accept
                (y-or-n-p "Generate an env file? (see `doom help env` for details)"))
        (doom-reload-env-file 'force-p)))

    ;; Install Doom packages
    (if (member "--no-install" args)
        (print! (warn "- Not installing plugins, as requested"))
      (print! "Installing plugins")
      (doom-packages-install doom-auto-accept))

    (print! "Regenerating autoloads files")
    (doom-reload-autoloads nil 'force-p)

    (if (member "--no-fonts" args)
        (print! (warn "- Not installing fonts, as requested"))
      (when (or doom-auto-accept
                (y-or-n-p "Download and install all-the-icon's fonts?"))
        (require 'all-the-icons)
        (let ((window-system (cond (IS-MAC 'ns)
                                   (IS-LINUX 'x))))
          (all-the-icons-install-fonts 'yes))))

    (print! (success "\nFinished! Doom is ready to go!\n"))
    (with-temp-buffer
      (doom-template-insert "QUICKSTART_INTRO")
      (print! (buffer-string)))))
