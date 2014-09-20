;; Variables
(defvar my-run-code-interpreter      nil)
(defvar my-run-code-func             'my/run-code-shell)
(defvar my-run-code-region-func      'my/run-code-region-shell)
(defvar my-switch-to-repl-func       nil)
(defvar my-send-region-to-repl-func  nil)
(defvar my-build-func                nil)

(make-variable-buffer-local 'my-run-code-interpreter)
(make-variable-buffer-local 'my-run-code-func)
(make-variable-buffer-local 'my-run-code-region-func)
(make-variable-buffer-local 'my-switch-to-repl-func)
(make-variable-buffer-local 'my-send-region-to-repl-func)
(make-variable-buffer-local 'my-build-func)

(add-hook! 'emacs-lisp-mode-hook
           (setq my-run-code-func 'eval-buffer
                 my-run-code-region-func 'eval-region))

(defun my/run-code-shell (file-path)
  (if (stringp my-run-code-interpreter)
      (shell-command (concat my-run-code-interpreter " " file-path))
    (message "No interpreter set for %s. See `my-run-code-interpreter'" (symbol-name major-mode))))

(defun my/run-code-region-shell (beg end)
  (if (stringp my-run-code-interpreter)
      (shell-command-on-region beg end my-run-code-interpreter)
    (message "No interpreter set for %s. See `my-run-code-interpreter'" (symbol-name major-mode))))

(defun my:switch-to-repl ()
  (interactive)
  (if (functionp my-switch-to-repl-func)
      (funcall my-switch-to-repl-func)
    (message "No REPL was set for %s. See `my-switch-to-repl-func'" (symbol-name major-mode))))

(defun my:send-region-to-repl (beg end)
  (interactive "r")
  (if (functionp my-send-region-to-repl-func)
      (funcall my-send-region-to-repl-func beg end)
    (message "No region runner set for %s. See `my-send-region-to-repl-func'" (symbol-name major-mode))))

(defun my:run-code-buffer ()
  (interactive)
  (let ((file-name (buffer-file-name))
        (mode-name (symbol-name major-mode)))
    (if (and (not (buffer-modified-p))
             (file-exists-p file-name))
        (if (functionp my-run-code-func)
            (funcall my-run-code-func file-name)
          (message "No runner set for %s. See `my-run-code-func'" mode-name))
      (my:run-code-region (point-min) (point-max)))))

(defun my:run-code-region (beg end)
  (interactive "r")
  (if (functionp my-run-code-region-func)
      (funcall my-run-code-region-func beg end)
    (message "No region runner set for %s. See `my-run-code-region-func'" (symbol-name major-mode))))

(defun my:build (&optional arguments)
  (interactive)
  (if (functionp my-build-func)
      (funcall my-build-func arguments)
    (message "No build function set for %s. See `my-build-func'" (symbol-name major-mode))))
