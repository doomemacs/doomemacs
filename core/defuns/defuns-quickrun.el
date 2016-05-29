;;; defuns-quickrun.el

;;;; Code building ;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defvar doom--build-command '("make %s" . "Makefile"))
(make-variable-buffer-local 'doom--build-command)

;;;###autoload
(defun doom/set-build-command (command &optional file)
  (when (or (null file)
            (doom/project-has-files file))
    (setq doom--build-command `(,command . ,file))))

;;;###autoload (autoload 'doom:build "defuns-quickrun" nil t)
(evil-define-command doom:build (arg)
  "Call a build command in the current directory. If ARG is nil this function calls
`recompile', otherwise it calls `compile' passing ARG as build command."
  (interactive "<sh>")
  (when (null doom--build-command)
    (user-error "No build command was set"))
  (let ((build-file (cdr doom--build-command))
        (build-cmd (car doom--build-command))
        (project-dir (doom/project-root)))
    (if (or (null build-file) (f-exists? (f-expand build-file project-dir)))
        (if (or (symbolp build-cmd) (functionp build-cmd))
            (if (commandp build-cmd)
                (call-interactively build-cmd)
              (funcall build-cmd))
          (let ((default-directory project-dir))
            (compile (format build-cmd (or arg "")))))
      (error "Could not build!"))))

;;;; Code running ;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload (autoload 'doom:eval-buffer "defuns-quickrun" nil t)
(evil-define-command doom:eval-buffer ()
  "Evaluate the whole buffer."
  :move-point nil :repeat nil
  (interactive)
  (cond ((eq major-mode 'emacs-lisp-mode)
         (doom:eval-region (point-min) (point-max)))
        (t (quickrun))))

;;;###autoload (autoload 'doom:eval-region "defuns-quickrun" nil t)
(evil-define-operator doom:eval-region (beg end)
  "Evaluate a region and, if large enough, prints its output to a popup buffer (if an
elisp buffer). Otherwise forward the region to Quickrun."
  :move-point nil :repeat nil
  (interactive "<r>")
  (cond ((eq major-mode 'emacs-lisp-mode)
         (require 'pp)
         (let* ((pp-escape-newlines nil)
                (out (pp-to-string (eval (read (buffer-substring-no-properties beg end)))))
                (lines (length (s-lines out))))
           (if (< lines 5)
               (princ out t)
             (let ((buf (get-buffer-create "*eval*")))
               (with-current-buffer buf
                 (read-only-mode -1)
                 (setq-local scroll-margin 0)
                 (emacs-lisp-mode)
                 (erase-buffer)
                 (insert out)
                 (read-only-mode 1)
                 (goto-char (point-min)))
               (pop-to-buffer buf)))))
        (t (quickrun-region beg end))))

;;;###autoload (autoload 'doom:eval-region-and-replace "defuns-quickrun" nil t)
(evil-define-operator doom:eval-region-and-replace (beg end)
  (interactive "<r>")
  (cond ((eq major-mode 'emacs-lisp-mode)
         (kill-region beg end)
         (condition-case nil
             (prin1 (eval (read (current-kill 0)))
                    (current-buffer))
           (error (message "Invalid expression")
                  (insert (current-kill 0)))))
        (t (quickrun-replace-region beg end))))

(provide 'defuns-quickrun)
;;; defuns-quickrun.el ends here
