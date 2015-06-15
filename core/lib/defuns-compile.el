;;; defuns-compile.el

(! (require 'f))

(defun narf--compile-important-dirs ()
  (append (list narf-core-dir narf-contrib-dir)
          (list (concat narf-modules-dir "lib/")
                (concat narf-core-dir "lib/"))
          (f-directories narf-contrib-dir)
          (list narf-modules-dir narf-private-dir)))

;;;###autoload (autoload 'narf:compile-el "defuns-compile" nil t)
(evil-define-command narf:compile-el (&optional bang)
  :repeat nil
  (interactive "<!>")
  (when (eq major-mode 'emacs-lisp-mode)
    (byte-recompile-file (! (f-expand "core-vars.el" narf-core-dir)) t 0)
    (byte-recompile-file (! (f-expand "core-defuns.el" narf-core-dir)) t 0)
    (if (not bang)
        (byte-recompile-file (buffer-file-name) t 0)
      (byte-recompile-file (! (f-expand "init-load-path.el" narf-emacs-dir)) nil 0)
      (byte-recompile-file (! (f-expand "init.el" narf-emacs-dir)) nil 0)
      (dolist (dir (narf--compile-important-dirs))
        (byte-recompile-directory dir 0 nil)))))

;;;###autoload (autoload 'narf:compile-autoloads "defuns-compile" nil t)
(evil-define-command narf:compile-autoloads (&optional bang)
  :repeat nil
  (interactive "<!>")
  (defvar generated-autoload-file (! (f-expand "autoloads.el" narf-core-dir)))
  (apply #'update-directory-autoloads (narf--compile-important-dirs)))

(provide 'defuns-compile)
;;; defuns-compile.el ends here
