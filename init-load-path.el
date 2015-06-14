;;; init-load-path.el

(fset '! 'eval-when-compile)

(defconst narf-emacs-dir     user-emacs-directory)
(defconst narf-core-dir      (! (concat narf-emacs-dir "core/")))
(defconst narf-modules-dir   (! (concat narf-emacs-dir "modules/")))
(defconst narf-contrib-dir   (! (concat narf-emacs-dir "contrib/")))
(defconst narf-private-dir   (! (concat narf-emacs-dir "private/")))
(defconst narf-elpa-dir      (! (concat narf-emacs-dir ".cask/" emacs-version "/elpa/")))
(defconst narf-temp-dir      (! (concat narf-private-dir "cache/" (system-name) "/")))
(defconst narf-snippet-dirs  (! (list (concat narf-private-dir "snippets/")
                                      (concat narf-private-dir "templates/"))))

(! (defun --subdirs (path)
     (let ((result '())
           (paths (ignore-errors (directory-files path t "^[^.]" t))))
       (dolist (file paths)
         (when (file-directory-p file)
           (add-to-list 'result file)))
       result)))

;; Scan various folders to populate the load-dirs
(setq custom-theme-load-path
      (! (append (--subdirs (concat narf-private-dir "themes/"))
                 custom-theme-load-path)))
(setq load-path
      (! (require 'cask)
         (cask-initialize)
         (setq load-path (append (list narf-core-dir narf-contrib-dir narf-modules-dir narf-private-dir)
                                 (list (concat narf-core-dir "lib"))
                                 (list (concat narf-modules-dir "lib"))
                                 (--subdirs narf-contrib-dir)
                                 load-path))))

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (eq system-type 'windows-nt))

;;; init-load-path.el ends here
