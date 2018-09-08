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
      (print! (green "Done!")))
    ;; Create init.el
    (let ((init-file (expand-file-name "init.el" doom-private-dir)))
      (if (file-exists-p init-file)
          (print! (yellow "%sinit.el already exists. Skipping.") short-private-dir)
        (print! "Copying init.example.el to %s" short-private-dir)
        (copy-file (expand-file-name "init.example.el" doom-emacs-dir)
                   init-file)
        (print! (green "Done!"))))
    ;; Create config.el
    (let ((config-file (expand-file-name "config.el" doom-private-dir)))
      (if (file-exists-p config-file)
          (print! "%sconfig.el already exists. Skipping." short-private-dir)
        (print! "Deploying empty config.el file in %s" short-private-dir)
        (with-temp-file config-file (insert ""))
        (print! (green "Done!")))))
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
