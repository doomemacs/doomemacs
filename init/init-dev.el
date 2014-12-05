(provide 'init-dev)

(add-hook 'find-file-hook 'hl-todo-mode)

(use-package dash-at-point
  :commands (dash-at-point dash-at-point-with-docset)
  :if is-mac)

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

;;; Config modes
(use-package yaml-mode
  :defer t
  :config (add-hook 'yaml-mode-hook 'enable-tab-width-2))

(use-package json-mode
  :mode (("\\.json\\'" . json-mode)
         ("\\.jshintrc\\'" . json-mode)))

(use-package emr
  :commands (emr-initialize emr-show-refactor-menu)
  :init (add-hook 'prog-mode-hook 'emr-initialize)
  :config
  (progn
    (bind 'normal "gR" 'emr-show-refactor-menu)

    (after "evil" (evil-ex-define-cmd "ref[actor]" 'emr-show-refactor-menu))))


;; Variables
(defvar-local my-run-code-interpreter       nil)
(defvar-local my-run-code-func              'my--run-code-shell)
(defvar-local my-run-code-region-func       'my--run-code-region-shell)
(defvar-local my-switch-to-repl-func        nil)
(defvar-local my-send-region-to-repl-func   nil)
(defvar-local my-build-func                 nil)

;; usage
(defvar-local my-test-single-func nil)
(defvar-local my-test-fixture-func nil)
(defvar-local my-test-all-func nil)

(defun my--run-code-shell (file-path)
  (if (stringp my-run-code-interpreter)
      (message (concat my-run-code-interpreter " " file-path))
    (error "No interpreter set for %s. See `my-run-code-interpreter'"
           (symbol-name major-mode))))

(defun my--run-code-region-shell (beg end)
  (if (stringp my-run-code-interpreter)
      (shell-command-on-region beg end my-run-code-interpreter)
    (error "No interpreter set for %s. See `my-run-code-interpreter'"
           (symbol-name major-mode))))

(defun my-switch-to-repl ()
  (interactive)
  (if (functionp my-switch-to-repl-func)
      (funcall my-switch-to-repl-func)
    (error "No REPL was set for %s. See `my-switch-to-repl-func'"
           (symbol-name major-mode))))

(defun my-send-region-to-repl (beg end)
  (interactive "r")
  (if (functionp my-send-region-to-repl-func)
      (funcall my-send-region-to-repl-func beg end)
    (error "No region runner set for %s. See `my-send-region-to-repl-func'"
           (symbol-name major-mode))))

(defun my-run-code-buffer ()
  (interactive)
  (let ((mode-name (symbol-name major-mode)))
    (if (or (buffer-modified-p)
            (not (f-exists? buffer-file-name)))
        (my:run-code-region (point-min) (point-max))
      (if (functionp my-run-code-func)
          (funcall my-run-code-func buffer-file-name)
        (error "No runner set for %s. See `my-run-code-func'" mode-name)))))

(defun my-run-code-region (beg end)
  (interactive "r")
  (if (functionp my-run-code-region-func)
      (funcall my-run-code-region-func beg end)
    (error "No region runner set for %s. See `my-run-code-region-func'"
           (symbol-name major-mode))))

(defun my-build (&optional arguments)
  (interactive)
  (if (functionp my-build-func)
      (funcall my-build-func arguments)
    (error "No build function set for %s. See `my-build-func'"
           (symbol-name major-mode))))



(after "evil"
  ;; (evil-ex-define-cmd "test[s]" 'my-test-single-func)
  ;; (evil-ex-define-cmd "testf[ixture]" 'my-test-fixture-func)
  ;; (evil-ex-define-cmd "testa[ll]" 'my-test-all-func)

  (require 'evil-snipe)
  (global-evil-snipe-mode 1)

  (evil-snipe-surround-compatibility)
  (evil-define-key 'visual evil-surround-mode-map (kbd "S") 'evil-surround-region))
