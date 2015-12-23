;;; init-packages.el

(setq-default
 ;; stop package.el from being annoying. NARF relies entirely on Cask.
 package--init-file-ensured t
 package-enable-at-startup nil)

(defconst narf-emacs-dir     user-emacs-directory)
(defconst narf-core-dir      (concat narf-emacs-dir "core/"))
(defconst narf-modules-dir   (concat narf-emacs-dir "modules/"))
(defconst narf-contrib-dir   (concat narf-emacs-dir "contrib/"))
(defconst narf-private-dir   (concat narf-emacs-dir "private/"))
(defconst narf-packages-dir  (concat narf-emacs-dir ".packages/"))

(defconst narf-script-dir    (concat narf-emacs-dir "scripts/"))
(defconst narf-dropbox-dir   "~/Dropbox/")
(defconst narf-snippet-dirs  (list (concat narf-private-dir "snippets/")
                                   (concat narf-private-dir "templates/")))
(defconst narf-temp-dir
  (format "%scache/%s/%s.%s/"
          narf-private-dir (system-name)
          emacs-major-version emacs-minor-version))

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (eq system-type 'windows-nt))

;;;;;;;;;;;;;;;;

(eval-when-compile
  (add-to-list 'load-path "/usr/local/bin/cask")
  (defun --subdirs (path)
    (let ((result '())
          (paths (ignore-errors (directory-files path t "^[^.]" t))))
      (dolist (file paths)
        (when (file-directory-p file)
          (add-to-list 'result file)))
      result)))

;; Scan various folders to populate the load-dirs
(setq custom-theme-load-path
      (eval-when-compile
        (append (--subdirs (concat narf-private-dir "themes/"))
                custom-theme-load-path)))

(setq load-path
      (eval-when-compile
        (require 'cask)
        (cask-initialize)
        (setq load-path (append (list narf-core-dir narf-contrib-dir narf-modules-dir narf-private-dir)
                                (list (concat narf-core-dir "lib"))
                                (list (concat narf-modules-dir "lib"))
                                (--subdirs narf-contrib-dir)
                                (--subdirs (concat narf-packages-dir "el-get/"))
                                load-path))))

;;;;;;;;;;;;;;;;

(autoload 'use-package "use-package" "" nil 'macro)
(require 'dash)

(when (file-exists-p "~/.emacs.local.el")
  (load "~/.emacs.local.el"))

;;; init-packages.el ends here
