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

(defun my/expand-space ()
  "Insert a space ahead of the cursor"
  (interactive)
  (save-excursion (insert " ")))

(defun my/expand-backspace ()
  "Add a space before and ahead of the cursor"
  (interactive)
  (save-excursion (delete-char 1))
  (delete-backward-char 1))

(defun my/enable-hard-wrap()
  "Enable hard line wrapping"
  (interactive)
  (auto-fill-mode 1))

(defun my/byte-recompile ()
  "Byte compile init.el, ~/.emacs.d/init/* and ~/.emacs.d/elisp/*"
  (interactive)
  (byte-recompile-file (expand-file-name "init.el" my/dir))
  (byte-recompile-directory my/init-dir 0)
  (byte-recompile-directory my/elisp-dir 0))

(defun my/notes()
  "Load up my notes folder in dropbox"
  (interactive)
  (ido-find-file-in-dir "~/Dropbox/notes"))

(defun my/kill-all-buffers ()
  "Kill all buffers, even the one you're in"
  (interactive)
  (mapc 'kill-buffer (buffer-list))
  (message "All buffers killed"))

(defun my/kill-other-buffers ()
  "Kill all buffers but the one you're in"
  (interactive)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer))))
  (message "All other buffers killed"))

(defun my/kill-non-project-buffers ()) ; TODO Implement this

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

;;;; Ac-setup Defuns ;;;;;;;;;;;;;;
(defun my/ac-ruby-setup()
  "Set up RSense and ac-sources"
  (setq ac-sources (append '(ac-source-rsense ac-source-yasnippet) ac-sources)))

(defun my/ac-files-setup()
  "Set up filepath completion sources"
  (setq ac-sources (append '(ac-source-filename ac-source-files-in-current-dir) ac-sources)))

(defun my/setup-run-code(mode interpreter)
  "Set up s-r to run code using a specified interpreter and print the
output in the echo area"
  (interactive)
  (nmap mode (kbd "s-r")
        `(lambda() (interactive) (shell-command-on-region (point-min) (point-max) ,interpreter)))
  (vmap mode (kbd "s-r")
        `(lambda() (interactive) (shell-command-on-region (region-beginning) (region-end) ,interpreter))))

;;;; Tmux defuns ;;;;;;;;;;;;;;;;;
(defun my/tmux-send(command)
  (interactive "sRun command: ")
  (shell-command (concat "tmux send-keys C-u " (shell-quote-argument command) " Enter"))
  (message "Tmux: Command sent!"))

(defun my/tmux-chdir(dir)
  (interactive "DDirectory: ")
  (shell-command (concat "tmux send-keys C-u \"cd " dir "\" Enter"))
  (message "Tmux: Directory changed!"))

;;;; Mac-specific Defuns ;;;;;;;;;
(when is-mac
  ;; Send current file to OSX apps
  (defun open-file-with (path &optional appName)
    (if (not (string= "" appName))
        (setq appName (concat "-a " appName ".app")))
    (shell-command (concat "open " appName " " path)))

  (defun open-with (appName)
    (interactive)
    (open-file-with (buffer-file-name) appName))

  (defun send-to-transmit ()      (interactive) (open-with "Transmit"))
  (defun send-to-launchbar ()     (interactive) (open-with "LaunchBar"))
  (defun send-dir-to-launchbar () (interactive) (open-file-with default-directory "LaunchBar"))
  (defun send-dir-to-finder ()    (interactive) (open-file-with default-directory "Finder")))

;;
(provide 'my-defuns)
