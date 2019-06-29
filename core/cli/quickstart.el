;;; core/cli/quickstart.el -*- lexical-binding: t; -*-

(dispatcher! (quickstart qs) (apply #'doom-quickstart args)
  "Guides you through setting up Doom for first time use.

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

  doom -p ~/.config/doom quickstart
  DOOMDIR=~/.config/doom doom quickstart

Quickstart understands the following switches:

  --no-config    Don't create DOOMDIR or dummy files therein
  --no-install   Don't auto-install packages
  --no-env       Don't generate an envvars file (see `doom help env`)
  --no-fonts     Don't install (or prompt to install) all-the-icons fonts")


;;
;; Library

(defun doom-quickstart (&rest args)
  "Quickly deploy a private module and setup Doom.

This deploys a barebones config to `doom-private-dir', installs all missing
packages, prompts to install all-the-icons fonts, generates an env file and
regenerates the autoloads file."
  ;; Create `doom-private-dir'
  (let ((short-private-dir (abbreviate-file-name doom-private-dir)))
    (if (member "--no-config" args)
        (print! (yellow "Not copying private config template, as requested"))
      (print! "Creating %s" short-private-dir)
      (make-directory doom-private-dir t)
      (print! (green "Done!"))

      ;; Create init.el, config.el & packages.el
      (dolist (file (list (cons "init.el"
                                (lambda ()
                                  (insert-file-contents (expand-file-name "init.example.el" doom-emacs-dir))))
                          (cons "config.el"
                                (lambda ()
                                  (insert (format ";;; %sconfig.el -*- lexical-binding: t; -*-\n\n"
                                                  short-private-dir)
                                          ";; Place your private configuration here\n")))
                          (cons "packages.el"
                                (lambda ()
                                  (insert (format ";; -*- no-byte-compile: t; -*-\n;;; %spackages.el\n\n"
                                                  short-private-dir)
                                          ";;; Examples:\n"
                                          ";; (package! some-package)\n"
                                          ";; (package! another-package :recipe (:fetcher github :repo \"username/repo\"))\n"
                                          ";; (package! builtin-package :disable t)\n")))))
        (cl-destructuring-bind (path . fn) file
          (if (file-exists-p! path doom-private-dir)
              (print! "%s already exists, skipping" path)
            (print! "Creating %s%s" short-private-dir path)
            (with-temp-file (expand-file-name path doom-private-dir)
              (funcall fn))
            (print! (green "Done!")))))))

  ;; In case no init.el was present the first time `doom-initialize-modules' was
  ;; called in core.el (e.g. on first install)
  (doom-initialize-packages 'force-p)

  ;; Ask if Emacs.app should be patched
  (if (member "--no-env" args)
      (print! (yellow "Not generating envvars file, as requested"))
    (when (or doom-auto-accept
              (y-or-n-p "Generate an env file? (see `doom help env` for details)"))
      (doom-reload-env-file 'force-p)))

  ;; Install Doom packages
  (if (member "--no-install" args)
      (print! (yellow "Not installing plugins, as requested"))
    (print! "Installing plugins")
    (doom-packages-install doom-auto-accept))

  (print! "Regenerating autoloads files")
  (doom-reload-autoloads nil 'force-p)

  (if (member "--no-fonts" args)
      (print! (yellow "Not installing fonts, as requested"))
    (when (or doom-auto-accept
              (y-or-n-p "Download and install all-the-icon's fonts?"))
      (require 'all-the-icons)
      (let ((window-system (cond (IS-MAC 'ns)
                                 (IS-LINUX 'x))))
        (all-the-icons-install-fonts 'yes))))

  (print! (bold (green "\nFinished! Doom is ready to go!\n")))
  (with-temp-buffer
    (doom-template-insert "QUICKSTART_INTRO")
    (print! (buffer-string))))
