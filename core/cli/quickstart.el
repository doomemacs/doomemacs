;;; core/cli/quickstart.el -*- lexical-binding: t; -*-

(dispatcher! (quickstart qs) (apply #'doom-quickstart args)
  "Quickly deploy a private module and Doom.

This deploys a barebones config to ~/.doom.d (if it doesn't already exist). The
destination can be changed with the -p option, e.g.

  doom -p ~/.config/doom quickstart

Quickstart understands the following switches:

  --no-config    Don't deploy dummy config to ~/.doom.d
  --no-install   Don't auto-install packages
  --no-env       Don't generate an envvars file (see `doom help env`)
  --no-fonts     Don't install (or prompt to install) all-the-icons fonts

This command is idempotent and is safe to reuse.")


;;
;; Library

(defun doom-quickstart (&rest args)
  "Quickly deploy a private module and Doom.

This deploys a barebones config to `doom-private-dir', installs all missing
packages and regenerates the autoloads file."
  ;; Create `doom-private-dir'
  (let ((short-private-dir (abbreviate-file-name doom-private-dir)))
    (if (member "--no-config" args)
        (print! (yellow "Not copying private config template, as requested"))
      (if (file-directory-p doom-private-dir)
          (print! (yellow "%s directory already exists. Skipping.") short-private-dir)
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
            (print! "Creating %s%s" short-private-dir path)
            (with-temp-file (expand-file-name path doom-private-dir)
              (funcall fn))
            (print! (green "Done!")))))))

  ;; In case no init.el was present the first time `doom-initialize-modules' was
  ;; called in core.el (e.g. on first install)
  (doom-initialize-modules)

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
      (all-the-icons-install-fonts 'yes)))

  (print! (bold (green "\nFinished! Doom is ready to go!\n")))
  (with-temp-buffer
    (doom-template-insert "QUICKSTART_INTRO")
    (print! (buffer-string))))
