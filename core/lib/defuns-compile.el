;;; defuns-compile.el

;; (eval-when-compile (require 'core))

;;;###autoload
(defun narf/is-recompilable-p ()
  "Does an .elc file exist and is this file in the .emacs.d folder?"
  (let ((file-name (buffer-file-name)))
    ;; TODO Detect init.el and init-load-path.el
    (and (f-exists? (f-expand (concat (f-base file-name) ".elc") (f-dirname file-name)))
         (--any? (f-child-of? file-name it)
                 (append (list narf-core-dir narf-contrib-dir)
                         (list (concat narf-modules-dir "lib/")
                               (concat narf-core-dir "lib/"))
                         (list narf-modules-dir narf-private-dir))))))

;;;###autoload (autoload 'narf:compile-el "defuns-compile" nil t)
(evil-define-command narf:compile-el (&optional bang)
  :repeat nil
  (interactive "<!>")
  (byte-recompile-file (f-expand "init.el" narf-emacs-dir) nil 0)
  (byte-recompile-file (f-expand "init-load-path.el" narf-emacs-dir) nil 0)
  (byte-recompile-file (f-expand "core.el" narf-core-dir) t 0)
  (if (and (eq major-mode 'emacs-lisp-mode) (not bang))
      (byte-recompile-file (buffer-file-name) t 0)
    (load (concat narf-script-dir "byte-compile.el"))))

;;;###autoload (autoload 'narf:compile-autoloads "defuns-compile" nil t)
(evil-define-command narf:compile-autoloads (&optional bang)
  :repeat nil
  (interactive "<!>")
  (load (concat narf-script-dir "generate-autoloads.el")))

(provide 'defuns-compile)
;;; defuns-compile.el ends here
