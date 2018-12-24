;;; core/cli/quickstart.el -*- lexical-binding: t; -*-

(dispatcher! (quickstart qs) (doom-quickstart)
  "Quickly deploy a private module and Doom.

This deploys a barebones config to ~/.doom.d. The destination can be changed
with the -p option, e.g.

  doom -p ~/.config/doom quickstart

This command will refuse to overwrite the private directory if it already
exists.")


;;
;; Library

(defun doom-quickstart ()
  "Quickly deploy a private module and Doom.

This deploys a barebones config to `doom-private-dir', installs all missing
packages and regenerates the autoloads file."
  ;; Create `doom-private-dir'
  (let ((short-private-dir (abbreviate-file-name doom-private-dir)))
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
          (print! (green "Done!"))))))
  ;; Ask if Emacs.app should be patched
  (when IS-MAC
    (message "MacOS detected")
    (condition-case e
        (doom-patch-macos nil (doom--find-emacsapp-path))
      (user-error (message "%s" (error-message-string e)))))
  ;; Install Doom packages
  (print! "Installing plugins")
  (doom-packages-install doom-auto-accept)
  (print! "Regenerating autoloads files")
  (doom-reload-autoloads nil 'force-p)
  (print! (bold (green "\nFinished! Doom is ready to go!\n")))
  (with-temp-buffer
    (doom-template-insert "QUICKSTART_INTRO")
    (print! (buffer-string))))
