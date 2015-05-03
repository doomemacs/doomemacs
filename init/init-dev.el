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
  :init (add-hook 'prog-mode-hook 'emr-initialize)
  :config
  (progn
    (bind 'normal "gR" 'emr-show-refactor-menu)
    (bind popup-menu-keymap [escape] 'keyboard-quit)

    (after "evil" (evil-ex-define-cmd "ref[actor]" 'emr-show-refactor-menu))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code building
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind my-mode-map "M-b" 'my:build)

(defvar my-build-command "make %s")
(make-variable-buffer-local 'my-build-command)
(add-hook! 'enh-ruby-mode-hook (setq my-build-command "rake %s"))

(evil-ex-define-cmd "ma[ke]" 'my:build)
(evil-define-command my:build (arg)
  "Call a build command in the current directory.
If ARG is nil this function calls `recompile', otherwise it calls
`compile' passing ARG as build command."
  (interactive "<sh>")
  (let ((makepath (f-traverse-upwards
                   (lambda (path)
                     (f-exists? (f-expand "Makefile" path)))
                   default-directory)))
    (if makepath
        (compile (format "cd '%s' && %s" makepath (format my-build-command (or arg ""))))
      (error "Could not find Makefile"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code running
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(evil-define-operator my:eval-region (beg end)
  :keep-visual t
  :move-point nil
  (interactive "<r>")
  (let ((interp (-my-get-interpreter)))
    (when interp (shell-command-on-region beg end interp))))

(evil-define-command my:eval-buffer ()
  (interactive)
  (let ((interp (-my-get-interpreter)))
    (when interp (shell-command-on-region (point-min) (point-max) interp))))

(bind 'motion "gr"   'my:eval-region
              "M-r"  'my:eval-region)
(bind 'normal "gR"   'my:eval-buffer
              "M-R"  'my:eval-buffer)

(defun -my-get-interpreter ()
  (car (--first (eq (cdr it) major-mode) interpreter-mode-alist)))

(provide 'init-dev)
;;; init-dev.el ends here
