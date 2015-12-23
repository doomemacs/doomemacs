#!emacs --script

(load (concat user-emacs-directory "init-packages.el"))

(require 'f)
(setq generated-autoload-file (concat narf-core-dir "autoloads.el"))
(when (f-exists? generated-autoload-file)
  (delete-file generated-autoload-file))

(let ((dirs (list (concat narf-core-dir "lib")
                  (concat narf-modules-dir "lib")
                  narf-contrib-dir)))
  (apply #'update-directory-autoloads dirs)
  (message "Scanned: %s" dirs))

