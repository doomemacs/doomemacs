;;;; Modes 'n hooks ;;;;;;;;;;;;;;;;;
(associate-mode "/LICENSE[^/]*$"                     'text-mode)
(associate-mode "zsh\\(env\\|rc\\)?$"                'sh-mode)
(associate-mode "z\\(profile\\|login\\|logout\\)?$"  'sh-mode)
(associate-mode "zsh/"                               'sh-mode)
(associate-mode "\\.applescript$"                    'applescript-mode)
(associate-mode "Cask$"                              'emacs-lisp-mode)
(associate-mode "\\.el\\.gz$"                        'emacs-lisp-mode)
(associate-mode "/Makefile$"                         'makefile-gmake-mode)
(associate-mode "\\.plist$"                          'nxml-mode)

;; (setenv "SHELL" (s-trim (shell-command-to-string "which zsh")))
(setenv "SHELL" "/usr/local/bin/zsh")
(setenv "EMACS" "1")

;; show-paren faces
(set-face-background 'show-paren-match nil)
(set-face-foreground 'show-paren-match "orange")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

(set-register ?. "~/.dotfiles/")
(set-register ?d "~/Dropbox/Projects/")
(set-register ?@ "~/.emacs.d/init.el")

(add-hook 'help-mode-hook 'visual-line-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Performance checks
(add-hook! 'find-file-hook
  ;; If file is oversized...
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)
    (visual-line-mode)))


;;;; Keymap Fixes ;;;;;;;;;;;;;;;;;;;;;;
;; Implements some helpful keymappings for emacs sub-modes
(add-hook! 'ido-setup-hook
           (bind ido-completion-map
                 (kbd "<backspace>")  'ido-delete-backward-updir
                 "\C-w"               'ido-delete-backward-word-updir))

;; Make ESC quit all the things
;; (bind minibuffer-inactive-mode-map [escape] (λ (other-window 1)))
(dolist (map (list minibuffer-local-map
                   minibuffer-local-ns-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-isearch-map))
  (bind map [escape] 'my--minibuffer-quit))
(bind 'emacs [escape] 'my--minibuffer-quit)
(bind 'normal evil-command-window-mode-map [escape] 'kill-buffer-and-window)
;; (bind evil-ex-map [escape] 'my--minibuffer-quit)

(dolist (map (list evil-ex-search-keymap minibuffer-local-map))
  (bind map "\C-w" 'evil-delete-backward-word))

(bind minibuffer-local-map
      "\C-u" 'evil-delete-whole-line)

;; Redefine to get rid of that silly delete-other-windows nonsense
(defun keyboard-escape-quit ()
  (interactive)
  (cond ((eq last-command 'mode-exited) nil)
        ((region-active-p)
         (deactivate-mark))
        ((> (minibuffer-depth) 0)
         (abort-recursive-edit))
        (current-prefix-arg
         nil)
        ((> (recursion-depth) 0)
         (exit-recursive-edit))
        (buffer-quit-function
         (funcall buffer-quit-function))
        ((string-match "^ \\*" (buffer-name (current-buffer)))
         (bury-buffer))))

(defun my-emacs-is-not-kill ()
  (interactive)
  (message "Gee, I dunno Brain..."))

(if is-mac (global-set-key (kbd "M-q") 'my-emacs-is-not-kill))


(provide 'my-settings)
;;; my-settings.el ends here
