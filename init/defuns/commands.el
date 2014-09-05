;;;; Defun Commands ;;;;;;;;;;;;;;;;;;;;

;; File navigation defuns
(defun my:goto-symbol (&optional symbol-list)
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
               (my:ido-goto-symbol (imenu--make-index-alist))
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
          (my:ido-goto-symbol symbol))
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

;; Buffer defuns
(defun my:kill-all-buffers ()
  "Kill all buffers, even the one you're in"
  (interactive)
  (delete-other-windows)
  (mapc 'kill-buffer (buffer-list))
  (message "All buffers killed"))

(defun my:kill-other-buffers ()
  "Kill all buffers but the one you're in"
  (interactive)
  (delete-other-windows)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer))))
  (message "All other buffers killed"))

(defun my:kill-dired-buffers ()
	 (interactive)
	 (mapc (lambda (buffer)
           (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
             (kill-buffer buffer)))
         (buffer-list)))

;;;; Tmux defuns ;;;;;;;;;;;;;;;;;
(defun my:tmux-run (command)
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
  (message "[Tmux] Command sent: %s" command))

(defun my:tmux-paste (command)
  (interactive "sSend to Tmux: ")
  (shell-command (concat "/usr/local/bin/tmux send-keys " (shell-quote-argument command)))
  (message "[Tmux] Text pasted: %s" command))

(defun my:tmux-chdir (dir)
  "CD into a new directory in tmux"
  (interactive "DDirectory: ")
  (my:tmux-run (concat "cd " (shell-quote-argument dir)))
  (message "[Tmux] Directory changed: %s" dir))

(defun my/project-root (&optional force-pwd)
  (if (and (not force-pwd)
           (projectile-project-p))
      (projectile-project-root)
    default-directory))

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


;;;; Ex-commands ;;;;;;;;;;;;;;;;;;;;;;;
(evil-define-command my:ex:msg-buffer () :repeat nil
  (interactive)
  (view-echo-area-messages)
  (text-mode))

(evil-define-command my:ex:kill-buffers (&optional bang) :repeat nil
  (interactive "<!>")
  (if bang (my:kill-all-buffers) (my:kill-other-buffers)))

(evil-define-command my:ex:init-files (&optional bang) :repeat nil
  (interactive "<!>")
  (if bang
      (ido-find-file-in-dir *init-dir)
    (ido-find-file-in-dir *dir)))

(evil-define-command my:ex:notes () :repeat nil
  (interactive)
  (ido-find-file-in-dir org-directory))

(evil-define-command my:ex:snippets (&optional bang) :repeat nil
  (interactive "<!>")
  (if bang
      (yas-new-snippet)
    (yas-visit-snippet-file)))

;; Projects
(evil-define-command my:ex:ag-search (search &optional bang) :repeat nil
  (interactive "<a><!>")
  (let ((root (my/project-root bang)))
    (ag search root)))

(evil-define-command my:ex:ag-regex-search (search &optional bang) :repeat nil
  (interactive "<a><!>")
  (let ((root (my/project-root bang)))
    (ag-regexp search root)))

(evil-define-command my:ex:tmux-chdir (&optional bang) :repeat nil
  (interactive "<!>")
  (if bang
      (my:tmux-chdir default-directory)
    (my:tmux-chdir (projectile-project-root))))

;; Run a command. If <bang>, then only type command into tmux
(evil-define-command my:ex:tmux-send (command &optional bang) :repeat nil
  (interactive "<fsh><!>")
  (if bang
      (my:tmux-paste command)
    (my:tmux-run command)))

(evil-define-operator my:ex:scratch-buffer (beg end &optional bang)
  :motion nil
  :move-point nil
  :type line
  :repeat nil
  (interactive "<r><!>")
  (let ((text nil)
        (mode major-mode)
        (text-empty-p nil))
    (when (and beg end)
      (setq text (buffer-substring beg end)))
    (if bang
        (if text
            (org-capture-string text)
          (org-capture))
      (progn
        (switch-to-buffer (get-buffer-create "*scratch*"))
        (if text (insert text))
        (funcall mode)))))

(evil-define-operator my:ex:retab (beg end)
  :motion nil
  :move-point nil
  :type line
  :repeat nil
  "Akin to vim's :retab, this changes all tabs-to-spaces or
spaces-to-tabs, depending on `indent-tab-mode'. Untested."
  (interactive "<r>")
  (let ((b beg)
        (e end))
    (unless (and b e)
      (setq b (point-min))
      (setq e (point-max)))
    (if indent-tabs-mode
        (tabify b e)
      (untabify b e))))

(evil-define-command my:ex:byte-compile-all (&optional bang) :repeat nil
  (interactive "<!>")
  (byte-recompile-file (expand-file-name "init.el" *dir) bang 0)
  (byte-recompile-directory *init-dir 0 bang)
  (byte-recompile-directory *elisp-dir 0 bang))

(evil-define-command my:ex:mru () :repeat nil
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file (find-file file))))
