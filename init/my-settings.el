(provide 'my-settings)

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
(add-hook! 'before-save-hook
  (unless (eq major-mode 'org-mode)
    (delete-trailing-whitespace)))

;; Performance checks
(add-hook! 'find-file-hook
  ;; If file is oversized...
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)
    (visual-line-mode)))


;;;; Keymap Fixes ;;;;;;;;;;;;;;;;;;;;;;
;; This section is dedicated to keymaps that "fix" certain keys so
;; that they behave more like vim (or how I like it).

;; Restores "dumb" indentation to the tab key. This rustles a lot of
;; peoples' jimmies, apparently, but it's how I like it.
(bind 'insert (kbd "TAB") 'my.dumb-indent)
;; Except for lisp
(bind 'insert lisp-mode-map        [remap my.dumb-indent] 'indent-for-tab-command)
(bind 'insert emacs-lisp-mode-map  [remap my.dumb-indent] 'indent-for-tab-command)

;; Highjacks backspace and space to:
;;   a) expand spaces between delimiters intelligently: (|) -> ( | )
;;   b) the reverse of A: ( | ) -> (|)
;;   c) allow backspace to delete indented blocks intelligently
;;   d) and not do any of this magic when inside a string
(bind 'insert
      (kbd "SPC")                              'my.inflate-space-maybe
      [remap backward-delete-char-untabify]    'my.deflate-space-maybe
      [remap newline]                          'my.newline-and-indent

      ;; Smarter move-to-beginning-of-line
      [remap move-beginning-of-line]           'my.move-to-bol

      ;; Restore bash-esque keymaps in insert mode; C-w and C-a already exist
      "\C-e" 'my.move-to-eol
      "\C-u" 'my.backward-kill-to-bol-and-indent

      ;; Fixes delete
      (kbd "<kp-delete>")   'delete-char

      ;; Textmate-esque insert-line before/after
      (kbd "<s-return>")    'evil-open-below
      (kbd "<S-s-return>")  'evil-open-above)

;; Fix osx keymappings and then some
(bind 'insert
      (kbd "<s-left>")      'my.move-to-bol
      (kbd "<s-right>")     'my.move-to-eol
      (kbd "<s-up>")        'beginning-of-buffer
      (kbd "<s-down>")      'end-of-buffer
      (kbd "<s-backspace>") 'my.backward-kill-to-bol-and-indent)

(add-hook! 'ido-setup-hook
           (bind ido-completion-map
                 (kbd "<backspace>")  'ido-delete-backward-updir
                 "\C-w"               'ido-delete-backward-word-updir))

;; Make ESC quit all the things
(bind (list minibuffer-local-map
            minibuffer-local-ns-map
            minibuffer-local-completion-map
            minibuffer-local-must-match-map
            minibuffer-local-isearch-map) [escape] 'my--minibuffer-quit)
(bind 'emacs [escape] 'my--minibuffer-quit)
(bind 'god [escape] 'evil-god-state-bail)
(bind 'normal evil-command-window-mode-map [escape] 'kill-buffer-and-window)
(bind evil-ex-map [escape] 'my--minibuffer-quit)

(bind (list evil-ex-search-keymap minibuffer-local-map)
      "\C-w" 'evil-delete-backward-word)
(bind minibuffer-local-map
      "\C-u" 'evil-delete-whole-line)
;; Close help/compilation windows with escape

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

(if is-mac (global-set-key (kbd "s-q") 'my-emacs-is-not-kill))
