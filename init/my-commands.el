(provide 'my-commands)

;;;; Defun Commands ;;;;;;;;;;;;;;;;;;;;
(defun my:minibuffer-quit ()
  "Abort recursive edit. In Delete Selection mode, if the mark is
active, just deactivate it; then it takes a second \\[keyboard-quit]
to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

;; Buffer defuns
(defun my:kill-other-buffers ()
  "Kill left-over temporary, dired or buried special buffers"
  (interactive)
  (mapc (lambda (buffer)
          (let ((buffer-name (buffer-name buffer)))
            (when (and (not (s-matches? buffer-name "\\*\\(scratch\\|Messages\\)\\*"))
                       (or (eq 'dired-mode (buffer-local-value 'major-mode buffer))
                           (s-matches? "^ ?\\*.+\\*$" buffer-name))
                       (not (get-buffer-window buffer)))
              (kill-buffer buffer))))
          (buffer-list)))

;; Inspired by http://demonastery.org/2013/04/emacs-evil-narrow-region/
(defun my:narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

(defun my:kill-persp ()
  (interactive)
  (persp-kill (persp-name persp-curr)))

;;;; Tmux defuns ;;;;;;;;;;;;;;;;;
(defun my:tmux-send (command)
  (interactive "sSend to Tmux: ")
  (shell-command
   (concat "/usr/local/bin/tmux send-keys " command)))

;;;; Mac-specific Defuns ;;;;;;;;;
(when is-mac
  ;; Send current file to OSX apps
  (defun my:open-file-with (path &optional appName)
    (if (and appName
             (stringp appName)
             (not (string= "" appName)))
        (setq appName (concat "-a " appName ".app")))
    (async-shell-command (concat "open " appName " " (shell-quote-argument path))))

  (defun my:open-with (appName)
    (interactive "sApp name: ")
    (my:open-file-with buffer-file-name appName))

  (defun my:send-to-transmit ()      (interactive) (my:open-with "Transmit"))
  (defun my:send-to-launchbar ()     (interactive) (my:open-with "LaunchBar"))
  (defun my:send-dir-to-launchbar () (interactive) (my:open-file-with default-directory "LaunchBar"))
  (defun my:send-dir-to-finder ()    (interactive) (my:open-file-with default-directory "Finder")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ex-commands                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(evil-define-command ex:kill-buffers (&optional bang) :repeat nil
  (interactive "<!>")
  (if (and bang (projectile-project-p))
      (projectile-kill-buffers)
    (mapc 'kill-buffer (buffer-list)))
  (delete-other-windows))

(evil-define-command ex:init-files (&optional bang) :repeat nil
  (interactive "<!>")
  (if bang
      (ido-find-file-in-dir *init-dir)
    (ido-find-file-in-dir *dir)))

(evil-define-command ex:notes () :repeat nil
  (interactive)
  (ido-find-file-in-dir org-directory))

;; Projects
(evil-define-command ex:ag-search (search &optional bang regex) :repeat nil
  (interactive "<a><!>")
  (let ((root (my/project-root bang)))
    (if search
        (if regex (ag-regexp search root) (ag search root))
      (helm-do-ag root))))

(evil-define-command ex:ag-regex-search (search &optional bang) :repeat nil
  (interactive "<a><!>")
  (ex:ag-search search bang t))

;; Run a command. If <bang>, then only type command into tmux
(evil-define-command ex:tmux-send (command &optional bang) :repeat nil
  (interactive "<sh><!>")
  (let ((cmd-format (if bang "%s" "C-u %s Enter")))
    (my:tmux-send (format cmd-format (shell-quote-argument command)))
    (when (evil-ex-p)
      (message "[Tmux] %s" command))))

(evil-define-command ex:tmux-chdir (&optional path bang) :repeat nil
  (interactive "<f><!>")
  (let ((dir (shell-quote-argument
              (if (and path
                       (not (s-blank? path))
                       (file-directory-p path))
                  (file-truename path)
                (my/project-root bang)))))
    (ex:tmux-send (format "cd %s" dir))
    (when (evil-ex-p)
      (message "[Tmux] cd %s" dir))))

(evil-define-command ex:byte-compile-all (&optional bang) :repeat nil
  (interactive "<!>")
  (require 'async-bytecomp)
  ;; (async-byte-recompile-directory *dir 0 bang)
  (byte-recompile-directory *dir 0 bang))

(evil-define-command ex:build (arguments &optional bang) :repeat nil
  (interactive "<a><!>")
  (my:build arguments))

(evil-define-command ex:cd (dir) :repeat nil
  (interactive "<f>")
  (cd (if (zerop (length dir)) "~" dir)))

(defun --save-exit() (save-buffer) (kill-buffer) (remove-hook 'yas-after-exit-snippet-hook '--save-exit))
(evil-define-command ex:create-file (path &optional bang) :repeat nil
  "Deploy files (and their associated templates) quickly. Will prompt
you to fill in each snippet field before buffer closes unless BANG is
provided."
  (interactive "<f><!>")
  (let ((dir (f-dirname path))
        (fullpath (f-full path))
        (is-auto t))
    (when (and bang (not (f-exists? dir))) (f-mkdir dir))
    (if (f-exists? dir)
        (if (f-exists? fullpath)
            (error "File already exists: %s" path)
          (find-file fullpath)
          (add-hook 'yas-after-exit-snippet-hook '--save-exit)
          (if bang (--save-exit)))
      (error "Directory doesn't exist: %s" dir))))

(evil-define-command ex:rename-this-file (new-name &optional bang) :repeat nil
  "Renames current buffer and file it is visiting."
  (interactive "<f><!>")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (if new-name new-name (read-file-name "New name: " filename))))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (when bang
            (delete-file filename))
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

;;;
(evil-define-operator ex:scratch-buffer (beg end)
  :move-point nil
  :type inclusive
  (interactive "<r>")
  (let ((mode major-mode)
        (text (when (and beg end) (buffer-substring beg end))))
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (if text (insert text))
    (funcall mode)))

(evil-define-operator ex:org-capture (beg end)
  :move-point nil
  :type inclusive
  (interactive "<r>")
  (let ((mode major-mode)
        (text (when (and beg end) (buffer-substring beg end))))
    (if text
        (org-capture-string text)
      (org-capture))))

(evil-define-operator ex:retab (beg end)
  :motion nil
  :move-point nil
  :type line
  "Akin to vim's :retab, this changes all tabs-to-spaces or
spaces-to-tabs, depending on `indent-tab-mode'. Untested."
  (interactive "<r>")
  (unless (and beg end)
    (setq beg (point-min))
    (setq end (point-max)))
  (if indent-tabs-mode
      (tabify beg end)
    (untabify beg end)))

(evil-define-operator ex:run-code (beg end)
  :move-point nil
  (interactive "<r>")
  (cond ((and beg end)
         (my:run-code-region beg end))
        (t
         (my:run-code-buffer))))

(evil-define-operator ex:send-region-to-repl (beg end &optional bang)
  :motion nil
  :move-point nil
  :type exclusive
  :repeat nil
  (interactive "<r><!>")
  (cond ((and beg end)
         (my:send-region-to-repl beg end))
        (t
         (my:switch-to-repl))))

(evil-define-operator ex:snippets (beg end &optional name)
  :motion nil
  :move-point nil
  :type exclusive
  :repeat nil
  "Create a new snippet (called `name'), or select from all the
  current mode's snippets to edit."
  (interactive "<r><a>")
  (cond ((and beg end)
         (yas-insert-snippet))
        (t
         (if name
             (find-file (concat *snippets-dir (symbol-name major-mode) "/" name))
           (yas-visit-snippet-file)))))

(evil-define-operator ex:narrow-indirect (beg end)
  :motion nil
  :move-point nil
  :type exclusive
  :repeat nil
  "Indirectly narrow the region from BEG to END."
  (interactive "<r>")
  (evil-normal-state)
  (narrow-to-region-indirect beg end))
