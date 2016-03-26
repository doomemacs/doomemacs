#!emacs --script
(load (concat user-emacs-directory "bootstrap.el"))

(let ((generated-autoload-file (concat narf-core-dir "/autoloads.el")))
  (when (file-exists-p generated-autoload-file)
    (delete-file generated-autoload-file))
  (mapc (lambda (dir)
          (update-directory-autoloads (concat dir "/defuns"))
          (message "Scanned: %s" dir))
        (list narf-core-dir narf-modules-dir))
  (message "Done!"))

