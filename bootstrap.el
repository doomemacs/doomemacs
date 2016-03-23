;;; bootstrap.el

(eval-and-compile
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
  (defconst IS-WINDOWS (eq system-type 'windows-nt)))

;; Global settings
(scroll-bar-mode -1)  ; no scrollbar
(tool-bar-mode   -1)  ; no toolbar

(setq-default
 ;; stop package.el from being annoying. NARF relies entirely on Cask.
 package--init-file-ensured t
 package-enable-at-startup nil
 gc-cons-threshold 4388608
 gc-cons-percentage 0.3)

(eval-and-compile
  ;; Make sure that cask is in the right place
  (unless (eq 0 (call-process "which" nil nil nil "cask"))
    (error "Cask could not be found"))
  (let ((cask-dir (cond (IS-MAC "/usr/local/share/emacs/site-lisp/cask")
                        (t "~/.cask"))))
    (unless (file-exists-p cask-dir)
      (error "Cask folder not found"))
    (push cask-dir load-path))

  ;; Helper for traversing subdirectories recursively
  (defun --subdirs (path &optional include-self)
    (let ((result (if include-self (list path) (list)))
          (paths (ignore-errors (directory-files path t "^[^.]" t))))
      (dolist (file paths)
        (when (file-directory-p file)
          (push file result)))
      result))

  (defvar narf--load-path load-path)
  (defun narf/reload ()
    (interactive)
    (setq load-path
          (progn (require 'cask)
                 (cask-initialize)
                 (append (list narf-private-dir)
                         (--subdirs narf-core-dir t)
                         (--subdirs narf-modules-dir t)
                         (--subdirs narf-packages-dir)
                         narf--load-path)))))

(defun narf (packages)
  "Bootstrap NARF emacs and initialize PACKAGES"
  ;; prematurely optimize for faster startup
  (let (file-name-handler-alist
        (gc-cons-threshold 169715200))
    ;; Scan various folders to populate the load-paths
    (setq custom-theme-load-path
          (append (list (expand-file-name "themes/" narf-private-dir))
                  custom-theme-load-path)
          load-path (eval-when-compile (narf/reload)))
    ;; Load local settings, if available
    (when (file-exists-p "~/.emacs.local.el")
      (load "~/.emacs.local.el"))
    (load-theme narf-default-theme t)
    (setq narf-current-theme narf-default-theme
          narf-current-font narf-default-font)
    (mapc 'require packages)

    (defun display-startup-echo-area-message ()
      (message ":: Loaded in %s" (emacs-init-time)))))

;;; bootstrap.el ends here
