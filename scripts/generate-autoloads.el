#!emacs --script
(load (concat user-emacs-directory "init-packages.el"))

(let ((generated-autoload-file (concat narf-core-dir "autoloads.el")))
  (when (file-exists-p generated-autoload-file)
    (delete-file generated-autoload-file))
  (mapc (lambda (dir)
          (update-directory-autoloads dir)
          (message "Scanned: %s" dir))
        (list (concat narf-core-dir "lib")
              (concat narf-modules-dir "lib")
              narf-contrib-dir))
  (message "Done!"))

