(use-package hl-todo
  :defer t
  :init (add-hook 'after-change-major-mode-hook 'hl-todo-mode))

(use-package dash-at-point
  :if is-mac
  :commands (dash-at-point dash-at-point-with-docset))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :config
  (progn
    (setq rainbow-delimiters-outermost-only-face-count 1)
    (set-face-attribute 'rainbow-delimiters-depth-1-face nil
                        :foreground 'unspecified
                        :inherit 'my-outermost-paren-face))
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'js2-mode-hook        'rainbow-delimiters-mode)
    (add-hook 'scss-mode-hook       'rainbow-delimiters-mode)))

;;; Config modes
(use-package yaml-mode
  :defer t
  :config (add-hook 'yaml-mode-hook 'enable-tab-width-2))

(use-package emr
  :commands (emr-initialize emr-show-refactor-menu)
  :init     (add-hook 'prog-mode-hook 'emr-initialize)
  :config   (bind popup-menu-keymap [escape] 'keyboard-quit))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code building
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-build-command '("make %s" . "Makefile"))
(make-variable-buffer-local 'my-build-command)

(defun set-build-command (command &optional file)
  (when (or (null file)
            (project-has-files file))
    (setq my-build-command `(,command . ,file))))

(evil-define-command my:build (arg)
  "Call a build command in the current directory.
If ARG is nil this function calls `recompile', otherwise it calls
`compile' passing ARG as build command."
  (interactive "<sh>")
  (when (null my-build-command)
    (user-error "No build command was set"))
  (let ((build-file (cdr my-build-command))
        (build-cmd (car my-build-command)))
    (if (project-has-files build-file)
        (compile (format "cd '%s' && %s" build-file (format build-cmd (or arg ""))))
      (error "Could not find Makefile"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code running
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(evil-define-operator my:eval-region (beg end)
  :move-point nil
  (interactive "<r>")
  (cond ((eq major-mode 'emacs-lisp-mode)
         (eval-region beg end))
        (t
         (let ((interp (my--get-interpreter))
               (max-mini-window-height 1))
           (when interp (shell-command-on-region beg end interp))))))

(evil-define-command my:eval-buffer ()
  (interactive)
  (cond ((eq major-mode 'emacs-lisp-mode)
         (eval-buffer))
        (t
         (let ((interp (my--get-interpreter))
               (max-mini-window-height 1))
           (when interp (shell-command-on-region (point-min) (point-max) interp))))))

(defun my--get-interpreter ()
  (car (--first (eq (cdr it) major-mode) interpreter-mode-alist)))

(provide 'init-dev)
;;; init-dev.el ends here
