;;;; Macros ;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro λ (&rest body)
  `(lambda () (interactive) ,@body))

;; vimmish keymapping shortcuts
(defmacro nmap (map &rest body)
  (macroexpand `(evil-define-key 'normal ,map ,@body)))
(defmacro vmap (map &rest body)
  (macroexpand `(evil-define-key 'visual ,map ,@body)))
(defmacro imap (map &rest body)
  (macroexpand `(evil-define-key 'insert ,map ,@body)))
(defmacro emap (map &rest body)
  (macroexpand `(evil-define-key 'emacs ,map ,@body)))
(defmacro nvmap (map &rest body)
  (macroexpand-all
   `(progn (nmap ,map ,@body)
           (vmap ,map ,@body))))

;; insert-mode key-chord mapping
(defmacro ichmap (key command)
  `(key-chord-define evil-insert-state-map ,key ,command))

(defmacro associate-mode (match mode &optional minor-mode-p)
  (let ((mode-alist (if minor-mode-p 'auto-minor-mode-alist 'auto-mode-alist)))
    `(add-to-list ',mode-alist '(,match . ,mode))))


;;;; Defuns ;;;;;;;;;;;;;;;;;;;;;;;;
(defun run-code-with (interpreter mode-map)
  "Set up ,r (and s-r on macs) to run code using a specified
interpreter and print the output in the echo area"
  (nmap mode-map (kbd ",r")
        `(lambda()
           (interactive)
           (if (and (not (buffer-modified-p))
                    (file-exists-p (buffer-file-name)))
               (shell-command (concat ,interpreter " " (buffer-file-name)))
             (shell-command-on-region (point-min) (point-max) ,interpreter))))
  (vmap mode-map (kbd ",r")
        `(lambda()
           (interactive)
           (shell-command-on-region (region-beginning) (region-end) ,interpreter)))

  (when is-mac
    (nmap mode-map (kbd "s-r") ",r")
    (vmap mode-map (kbd "s-r") ",r")))
