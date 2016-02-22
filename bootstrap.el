;;; bootstrap.el

(defconst narf-emacs-dir     user-emacs-directory)
(defconst narf-core-dir      (concat narf-emacs-dir "core/"))
(defconst narf-modules-dir   (concat narf-emacs-dir "modules/"))
(defconst narf-private-dir   (concat narf-emacs-dir "private/"))
(defconst narf-packages-dir  (concat narf-emacs-dir ".cask/" emacs-version "/elpa/"))
(defconst narf-script-dir    (concat narf-emacs-dir "scripts/"))
(defconst narf-snippet-dirs  (list (concat narf-private-dir "snippets/")
                                   (concat narf-private-dir "templates/")))
;; Hostname and emacs version-based elisp temp directories
(defconst narf-temp-dir
  (format "%scache/%s/%s.%s/"
          narf-private-dir (system-name)
          emacs-major-version emacs-minor-version))

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (eq system-type 'windows-nt))

;; Global settings
(scroll-bar-mode -1)  ; no scrollbar
(tool-bar-mode   -1)  ; no toolbar

(setq-default
 ;; stop package.el from being annoying. NARF relies entirely on Cask.
 package--init-file-ensured t
 package-enable-at-startup nil
 gc-cons-threshold 4388608
 gc-cons-percentage 0.3)

(eval-when-compile
  ;; Make sure that cask is in the right place
  (unless (eq 0 (call-process "which" nil nil nil "cask"))
    (error "Cask could not be found"))
  (let ((cask-dir (cond (IS-MAC "/usr/local/Cellar/cask/HEAD")
                        (t "~/.cask"))))
    (unless (file-exists-p cask-dir)
      (error "Cask folder not found"))
    (add-to-list 'load-path cask-dir))

  ;; Helper for traversing subdirectories recursively
  (defun --subdirs (path &optional include-self)
    (let ((result (if include-self (list path) (list)))
          (paths (ignore-errors (directory-files path t "^[^.]" t))))
      (dolist (file paths)
        (when (file-directory-p file)
          (push file result)))
      result)))

;; Scan various folders to populate the load-paths
(defun narf/reload ()
  (interactive)
  (setq custom-theme-load-path
        (eval-when-compile
          (append (--subdirs (concat narf-private-dir "themes/"))
                  custom-theme-load-path)))
  (setq load-path
        (eval-when-compile
          (require 'cask)
          (cask-initialize)
          (setq load-path (append (list narf-private-dir)
                                  (--subdirs narf-core-dir t)
                                  (--subdirs narf-modules-dir t)
                                  (--subdirs narf-packages-dir)
                                  load-path)))))
(narf/reload)

(defun narf (packages)
  "Bootstrap NARF emacs and initialize PACKAGES"
  ;; prematurely optimize for faster startup
  (let (file-name-handler-alist
        (gc-cons-threshold 169715200))
    ;; Load local settings, if available
    (when (file-exists-p "~/.emacs.local.el")
      (load "~/.emacs.local.el"))

    (load-theme narf-theme t)
    (setq narf-current-theme narf-theme)

    (mapc 'require packages)))

;;; bootstrap.el ends here
