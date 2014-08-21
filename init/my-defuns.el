;;;; Macros ;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro λ (&rest body)
  `(lambda () (interactive) ,@body))

;; vimmish keymapping shortcuts
(defmacro nmap (map &rest body)
  `(evil-define-key 'normal ,map ,@body))
(defmacro vmap (map &rest body)
  `(evil-define-key 'visual ,map ,@body))
(defmacro imap (map &rest body)
  `(evil-define-key 'insert ,map ,@body))
(defmacro emap (map &rest body)
  `(evil-define-key 'emacs ,map ,@body))
(defmacro nvmap (map &rest body)
  `(evil-define-key 'normal ,map ,@body)
  `(evil-define-key 'visual ,map ,@body))

;; insert-mode key-chord mapping
(defmacro ichmap (key command)
  `(key-chord-define evil-insert-state-map ,key ,command))

;;;; Commands ;;;;;;;;;;;;;;;;;;;;;;
;; File navigation defuns
(defun my/initfiles ()
  "Do an ido-find in ~/.emacs.d"
  (interactive)
  (ido-find-file-in-dir my/dir))

(defun my/open-scratch ()
  "Open a blank scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (text-mode))

(defun my/byte-recompile ()
  "Byte compile init.el, ~/.emacs.d/init/* and ~/.emacs.d/elisp/*"
  (interactive)
  (byte-recompile-file (expand-file-name "init.el" my/dir) t)
  (byte-recompile-directory my/init-dir 0 t)
  (byte-recompile-directory my/elisp-dir 0 t))

(defun my/notes()
  "Load up my notes folder in dropbox"
  (interactive)
  (ido-find-file-in-dir "~/Dropbox/notes"))

(defun my/kill-all-buffers ()
  "Kill all buffers, even the one you're in"
  (interactive)
  (delete-other-windows)
  (mapc 'kill-buffer (buffer-list))
  (message "All buffers killed"))

(defun my/kill-other-buffers ()
  "Kill all buffers but the one you're in"
  (interactive)
  (delete-other-windows)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer))))
  (message "All other buffers killed"))

(defun my/kill-dired-buffers ()
	 (interactive)
	 (mapc (lambda (buffer)
           (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
             (kill-buffer buffer)))
         (buffer-list)))

(defun my/recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun my/ido-goto-symbol (&optional symbol-list)
      "Refresh imenu and jump to a place in the buffer using Ido."
      (interactive)
      (unless (featurep 'imenu)
        (require 'imenu nil t))
      (cond
       ((not symbol-list)
        (let ((ido-mode ido-mode)
              (ido-enable-flex-matching
               (if (boundp 'ido-enable-flex-matching)
                   ido-enable-flex-matching t))
              name-and-pos symbol-names position)
          (unless ido-mode
            (ido-mode 1)
            (setq ido-enable-flex-matching t))
          (while (progn
                   (imenu--cleanup)
                   (setq imenu--index-alist nil)
                   (my/ido-goto-symbol (imenu--make-index-alist))
                   (setq selected-symbol
                         (ido-completing-read "Symbol? " symbol-names))
                   (string= (car imenu--rescan-item) selected-symbol)))
          (unless (and (boundp 'mark-active) mark-active)
            (push-mark nil t nil))
          (setq position (cdr (assoc selected-symbol name-and-pos)))
          (cond
           ((overlayp position)
            (goto-char (overlay-start position)))
           (t
            (goto-char position)))))
       ((listp symbol-list)
        (dolist (symbol symbol-list)
          (let (name position)
            (cond
             ((and (listp symbol) (imenu--subalist-p symbol))
              (my/ido-goto-symbol symbol))
             ((listp symbol)
              (setq name (car symbol))
              (setq position (cdr symbol)))
             ((stringp symbol)
              (setq name symbol)
              (setq position
                    (get-text-property 1 'org-imenu-marker symbol))))
            (unless (or (null position) (null name)
                        (string= (car imenu--rescan-item) name))
              (add-to-list 'symbol-names name)
              (add-to-list 'name-and-pos (cons name position))))))))

;;;; Hooks ;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/enable-hard-wrap()
  (auto-fill-mode 1)
  (diminish 'auto-fill-function))

(defun my/enable-comment-hard-wrap ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode 1)
  (diminish 'auto-fill-function))

(defun my/ac-ruby-setup()
  "Set up RSense and ac-sources"
  (setq ac-sources (append '(ac-source-rsense ac-source-yasnippet) ac-sources)))

(defun my/ac-files-setup()
  "Set up filepath completion sources"
  (setq ac-sources (append '(ac-source-filename ac-source-files-in-current-dir) ac-sources)))

(defun my/setup-run-code(mode interpreter)
  "Set up s-r to run code using a specified interpreter and print the
output in the echo area"
  (nmap mode (kbd ",r")
        `(lambda()
           (interactive)
           (if (file-exists-p (buffer-file-name))
               (shell-command (concat ,interpreter " " (buffer-file-name)))
             (shell-command-on-region (point-min) (point-max) ,interpreter))))
  (vmap mode (kbd ",r")
        `(lambda() (interactive) (shell-command-on-region (region-beginning) (region-end) ,interpreter))))

;;;; Tmux defuns ;;;;;;;;;;;;;;;;;
(defun my/tmux-run (command)
  "Run command in tmux"
  (interactive
   (list
    (read-shell-command "Tmux command: " nil nil
                        (let ((filename (cond (buffer-file-name)
                                              ((eq major-mode 'dired-mode)
                                               (dired-get-filename nil t)))))
                          (and filename (file-relative-name filename))))))

  (shell-command (concat "/usr/local/bin/tmux send-keys C-u " (shell-quote-argument command) " Enter"))
  ;; (call-process "/usr/local/bin/tmux" nil nil nil "C-u" "send-keys" command "C-m")
  (message "Tmux: Command sent!"))

(defun my/tmux-paste (command)
  (interactive "sSend to Tmux: ")
  (shell-command (concat "/usr/local/bin/tmux send-keys " command))
  (message "Tmux: Text pasted!"))

(defun my/tmux-chdir(dir)
  "CD into a new directory in tmux"
  (interactive "DDirectory: ")
  (my/tmux-run (concat "cd " (shell-quote-argument dir)))
  (message "Tmux: Directory changed!"))

;;;; Mac-specific Defuns ;;;;;;;;;
(when is-mac
  ;; Send current file to OSX apps
  (defun open-file-with (path &optional appName)
    (if (and appName
             (stringp appName)
             (not (string= "" appName)))
        (setq appName (concat "-a " appName ".app")))
    (shell-command (concat "open " appName " " (shell-quote-argument path))))

  (defun open-with (appName)
    (interactive "sApp name: ")
    (open-file-with buffer-file-name appName))

  (defun send-to-transmit ()      (interactive) (open-with "Transmit"))
  (defun send-to-launchbar ()     (interactive) (open-with "LaunchBar"))
  (defun send-dir-to-launchbar () (interactive) (open-file-with default-directory "LaunchBar"))
  (defun send-dir-to-finder ()    (interactive) (open-file-with default-directory "Finder")))

;;
(provide 'my-defuns)
