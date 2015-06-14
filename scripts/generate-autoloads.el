#!emacs --script

(setq user-emacs-directory (concat (file-name-directory load-file-name) "../"))
(load (concat user-emacs-directory "init-load-path.el"))

(require 'f)
(setq generated-autoload-file (concat narf-core-dir "autoloads.el"))
(when (f-exists? generated-autoload-file)
  (delete-file generated-autoload-file))

(apply #'update-directory-autoloads (list (concat narf-core-dir "lib")
                                          (concat narf-modules-dir "lib")
                                          narf-contrib-dir))
