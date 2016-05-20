;;; bootstrap.el

(eval-when-compile
  (defconst emacs-start-time (current-time))
  (require 'cl-lib))

;; Global constants
(eval-and-compile
  (defconst narf-default-theme  'wombat)
  (defconst narf-terminal-theme 'wombat)
  (defconst narf-default-font  nil)

  (defconst narf-emacs-dir     (expand-file-name "." user-emacs-directory))
  (defconst narf-core-dir      (concat narf-emacs-dir "/core"))
  (defconst narf-modules-dir   (concat narf-emacs-dir "/modules"))
  (defconst narf-private-dir   (concat narf-emacs-dir "/private"))
  (defconst narf-packages-dir  (concat narf-emacs-dir "/.cask/" emacs-version "/elpa"))
  (defconst narf-script-dir    (concat narf-emacs-dir "/scripts"))
  (defconst narf-ext-dir       (concat narf-emacs-dir "/ext"))
  (defconst narf-snippet-dirs  (list (concat narf-private-dir "/snippets")
                                     (concat narf-private-dir "/templates")))
  ;; Hostname and emacs version-based elisp temp directories
  (defconst narf-temp-dir      (format "%s/cache/%s/%s.%s"
                                       narf-private-dir (system-name)
                                       emacs-major-version emacs-minor-version))

  (defconst IS-MAC     (eq system-type 'darwin))
  (defconst IS-LINUX   (eq system-type 'gnu/linux))
  (defconst IS-WINDOWS (eq system-type 'windows-nt)))

(eval-when-compile
  (defvar narf--load-path load-path)

  ;; Helper for traversing subdirectories recursively
  (defun --subdirs (path &optional include-self)
    (let ((result (if include-self (list path) (list))))
      (dolist (file (ignore-errors (directory-files path t "^[^.]" t)))
        (when (file-directory-p file)
          (push file result)))
      result)))


;;
;; Bootstrap
;;

;; Shut up byte-compiler!
(defvar narf-current-theme)
(defvar narf-current-font)

(defun narf (packages)
  "Bootstrap NARF emacs and initialize PACKAGES"
  (setq-default gc-cons-threshold 4388608
                gc-cons-percentage 0.4)

  ;; prematurely optimize for faster startup
  (let ((gc-cons-threshold 339430400)
        (gc-cons-percentage 0.6)
        file-name-handler-alist)

    ;; Scan various folders to populate the load-paths
    (setq load-path
          (eval-when-compile
            (append (list narf-private-dir)
                    (--subdirs narf-core-dir t)
                    (--subdirs narf-modules-dir t)
                    (--subdirs narf-packages-dir)
                    (--subdirs (expand-file-name "../bootstrap" narf-packages-dir))
                    narf--load-path))
          custom-theme-load-path
          (append (list (expand-file-name "themes/" narf-private-dir))
                  custom-theme-load-path))

    ;; Local settings
    (load "~/.emacs.local.el" t t)
    (setq narf-current-theme (if (display-graphic-p) narf-default-theme narf-terminal-theme)
          narf-current-font narf-default-font)
    ;; Here we a'go!
    (mapc 'require packages)

    (when (display-graphic-p)
      (require 'server)
      (unless (server-running-p)
        (server-start)))))

;;; bootstrap.el ends here
