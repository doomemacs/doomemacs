;;; defuns-quickrun.el

;;;; Code building ;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defvar narf--build-command '("make %s" . "Makefile"))
(make-variable-buffer-local 'narf--build-command)

;;;###autoload
(defun narf/set-build-command (command &optional file)
  (when (or (null file)
            (narf/project-has-files file))
    (setq narf--build-command `(,command . ,file))))

;;;###autoload (autoload 'narf:build "defuns-quickrun" nil t)
(evil-define-command narf:build (arg)
  "Call a build command in the current directory.
If ARG is nil this function calls `recompile', otherwise it calls
`compile' passing ARG as build command."
  (interactive "<sh>")
  (when (null narf--build-command)
    (user-error "No build command was set"))
  (let ((build-file (cdr narf--build-command))
        (build-cmd (car narf--build-command)))
    (if (narf/project-has-files build-file)
        (compile (format "cd '%s' && %s" (narf/project-root) (format build-cmd (or arg ""))))
      (error "Could not find Makefile"))))

;;;; Code running ;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload (autoload 'narf:eval-buffer "defuns-quickrun" nil t)
(evil-define-command narf:eval-buffer ()
  :move-point nil
  (interactive)
  (cond ((eq major-mode 'emacs-lisp-mode)
         (narf:eval-region (point-min) (point-max)))
        (t (quickrun))))

;;;###autoload (autoload 'narf:eval-region "defuns-quickrun" nil t)
(evil-define-operator narf:eval-region (beg end)
  :move-point nil
  (interactive "<r>")
  (cond ((eq major-mode 'emacs-lisp-mode)
         (let* ((pp-escape-newlines nil)
                (out (s-trim (pp-to-string (eval (read (buffer-substring-no-properties beg end))))))
                (lines (length (s-lines out))))
           (if (< lines 5)
               (princ out t)
             (let ((buf (get-buffer-create "*eval*")))
               (with-current-buffer buf
                 (read-only-mode -1)
                 (emacs-lisp-mode)
                 (setq-local scroll-margin 0)
                 (erase-buffer)
                 (insert out)
                 (goto-char (point-min))
                 (read-only-mode 1)
                 (popwin:popup-buffer buf :height (if (> lines 25) 25 (1+ lines))))))))
        (t (quickrun-region beg end))))

;;;###autoload (autoload 'narf:eval-region-and-replace "defuns-quickrun" nil t)
(evil-define-operator narf:eval-region-and-replace (beg end)
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
