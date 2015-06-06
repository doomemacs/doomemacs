;;; defuns-compile.el

(! (require 'f)
   (setq narf-important-dirs (append (list narf-core-dir narf-modules-dir narf-contrib-dir narf-private-dir)
                                     (f-directories narf-core-dir nil t)
                                     (f-directories narf-modules-dir nil t)
                                     (f-directories narf-contrib-dir nil t))))

;;;###autoload (autoload 'narf:byte-compile "defuns-compile")
(evil-define-command narf:byte-compile (&optional bang)
  :repeat nil
  (interactive "<!>")
  (when emacs-lisp-mode
    (if (not bang)
        (progn
          (byte-recompile-file (! (f-expand "core-defuns.el" narf-core-dir)) t 0)
          (byte-recompile-file (buffer-file-name) t 0))
      (byte-recompile-file (! (f-expand "init.el" narf-emacs-dir)) nil 0)
      (byte-recompile-file (! (f-expand "startup.el" narf-emacs-dir)) nil 0)
      (dolist (dir (! narf-impotant-dirs))
        (byte-recompile-directory dir 0 nil)))))

;;;###autoload (autoload 'narf:autoload-compile "defuns-compile")
(evil-define-command narf:autoload-compile (&optional bang)
  :repeat nil
  (interactive "<!>")
  (defvar generated-autoload-file (! (f-expand "autoloads.el" narf-core-dir)))
  (apply #'update-directory-autoloads (! narf-impotant-dirs)))


(provide 'defuns-compile)
;;; defuns-compile.el ends here
