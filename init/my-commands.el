(provide 'my-commands)

;;;; Defun Commands ;;;;;;;;;;;;;;;;;;;;
(defun my:git-gutter-refresh ()
  (interactive)
  (git-gutter+-refresh))

(defun my:minibuffer-quit ()
  "Abort recursive edit. In Delete Selection mode, if the mark is
active, just deactivate it; then it takes a second \\[keyboard-quit]
to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

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
               (my:goto-symbol (imenu--make-index-alist))
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
          (my:goto-symbol symbol))
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
(defun my:kill-dired-buffers ()
	 (interactive)
	 (mapc (lambda (buffer)
           (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
             (kill-buffer buffer)))
         (buffer-list)))

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
    (shell-command (concat "open " appName " " (shell-quote-argument path))))

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

(evil-define-command my:ex:msg-buffer () :repeat nil
  (interactive)
  (view-echo-area-messages)
  (switch-to-buffer-other-window "*Messages*")
  ;; Force text-mode for unfettered evil-mode & my keymappings
  (text-mode))

(evil-define-command my:ex:kill-buffers (&optional bang) :repeat nil
  (interactive "<!>")
  (let ((buffers (if bang
                     (cdr (buffer-list (current-buffer)))
                   (buffer-list))))
    (delete-other-windows)
    (mapc 'kill-buffer buffers)))

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

;; Run a command. If <bang>, then only type command into tmux
(evil-define-command my:ex:tmux-send (command &optional bang) :repeat nil
  (interactive "<sh><!>")
  (let ((cmd-format (if bang "%s" "C-u %s Enter")))
    (my:tmux-send (format cmd-format (shell-quote-argument command)))
    (when (evil-ex-p)
      (message "[Tmux] %s" command))))

(evil-define-command my:ex:tmux-chdir (&optional path bang) :repeat nil
  (interactive "<f><!>")
  (let ((dir (shell-quote-argument
              (if (and path
                       (not (s-blank? path))
                       (file-directory-p path))
                  (file-truename path)
                (my/project-root bang)))))
    (my:ex:tmux-run (format "cd %s" dir))
    (when (evil-ex-p)
      (message "[Tmux] cd %s" dir))))

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

(evil-define-command my:ex:build (arguments &optional bang) :repeat nil
  (interactive "<a><!>")
  (my:build arguments))

(evil-define-command my:ex:cd (dir) :repeat nil
  (interactive "<f>")
  (cd (if (zerop (length dir)) "~" dir)))

;;;
(evil-define-operator my:ex:scratch-buffer (beg end)
  :motion nil
  :move-point nil
  :type inclusive
  :repeat nil
  (interactive "<r>")
  (let ((mode major-mode)
        (text (when (and beg end) (buffer-substring beg end))))
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (if text (insert text))
    (funcall mode)))

(evil-define-operator my:ex:org-capture (beg end)
  :motion nil
  :move-point nil
  :type inclusive
  :repeat nil
  (interactive "<r>")
  (let ((mode major-mode)
        (text (when (and beg end) (buffer-substring beg end))))
    (if text
        (org-capture-string text)
      (org-capture))))

(evil-define-operator my:ex:retab (beg end)
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

(evil-define-operator my:ex:run-code (beg end) :repeat nil
  :motion nil
  :move-point nil
  :type exclusive
  :repeat nil
  (interactive "<r>")
  (cond ((and beg end)
         (my:run-code-region beg end))
        (t
         (my:run-code-buffer))))

(evil-define-operator my:ex:send-region-to-repl (beg end &optional bang) :repeat nil
  :motion nil
  :move-point nil
  :type exclusive
  :repeat nil
  (interactive "<r><!>")
  (cond ((and beg end)
         (my:send-region-to-repl beg end))
        (t
         (my:switch-to-repl))))
