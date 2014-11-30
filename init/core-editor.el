(provide 'core-editor)

;;;; Editor behavior ;;;;;;;;;;;;;;;;
(setq sentence-end-double-space nil)
;; (setq require-final-newline nil)
;; (setq mode-require-final-newline nil)
(setq-default fill-column 80)
(electric-indent-mode -1)   ; In case of emacs 24.4
(setq-default tab-width 4
              tab-always-indent nil
              indent-tabs-mode nil)     ; spaces instead of tabs

;; Show tab characters
(global-whitespace-mode 1)
(setq whitespace-style '(face tabs tab-mark) ; needs to be re-set in every buffer
      whitespace-display-mappings
      '((tab-mark   ?\t   [?| ?\t] [?\\ ?\t])
        (newline-mark 10 [36 10])))

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

(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Kill special (\\*.+\\*) buffers that are buried
(run-with-idle-timer 10 t 'my:kill-other-buffers)


;;;; Evil ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package evil
  :pre-load (defvar evil-want-C-u-scroll t)
  :init
  (progn
    (evil-mode 1)

    (setq evil-want-visual-char-semi-exclusive t
          evil-search-module 'evil-search
          evil-magic 'very-magic

          ;; Color-coded state cursors
          evil-normal-state-cursor '("white" box)
          evil-visual-state-cursor '("cyan" box)
          evil-god-state-cursor '("orange" box))

    ;; Fixes C-i's synonymity with TAB
    (keyboard-translate ?\C-i ?\H-i)

    (add-hook! 'find-file-hook (setq evil-shift-width tab-width))

    ;; modes to map to different default states
    (dolist (mode-map '((cider-repl-mode . emacs)
                        (comint-mode . emacs)
                        (eshell-mode . emacs)
                        (fundamental-mode . normal)
                        (git-commit-mode . insert)
                        (git-rebase-mode . emacs)
                        (help-mode . normal)
                        (term-mode . emacs)))
      (evil-set-initial-state `,(car mode-map) `,(cdr mode-map)))

    (use-package evil-ex-registers)
    (use-package evil-exchange)
    (use-package evil-indent-textobject)    ; vii/vai/vaI
    (use-package evil-numbers)
    (use-package evil-visualstar)
    (use-package evil-nerd-commenter  :pre-load (setq evilnc-hotkey-comment-operator "g/"))
    (use-package evil-space           :init (evil-space-default-setup))
    (use-package evil-matchit         :init (global-evil-matchit-mode 1))
    (use-package evil-surround        :init (global-evil-surround-mode 1))
    (use-package evil-god-state)
    (use-package evil-jumper
      :pre-load (defvar evil-jumper-file (expand-file-name "jumplist" *tmp-dir))
      :config
      (progn
        (setq evil-jumper-auto-center t
              evil-jumper-auto-save-interval 3600)

        (define-key evil-motion-state-map (kbd "H-i") 'evil-jumper/forward)))

    ;;;; ace-jump ;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Enable half-cursor blink when using ace-jump
    (defadvice evil-ace-jump-char-mode (before evil-ace-jump-char-mode-op activate) (evil-half-cursor))
    (defadvice evil-ace-jump-word-mode (before evil-ace-jump-word-mode-op activate) (evil-half-cursor))
    (setq ace-jump-mode-scope 'global) ; limit ace-jump to current window
    (setq ace-jump-mode-move-keys      ; use a-z, 0-9
          (nconc (loop for i from ?a to ?z collect i)
                 (loop for i from ?A to ?Z collect i)))
    ;; (setq ace-jump-mode-move-keys      ; use a-z
    ;;       (loop for i from ?a to ?z collect i))

    ;; Exit evil-exchange mode with <Esc> (silently) -- and close
    ;; minibuffer remotely if it happens to be left open
    (defadvice evil-force-normal-state (before evil-esc-quit-exchange activate)
      (shut-up (evil-exchange-cancel)
               (if (minibuffer-window-active-p (minibuffer-window))
                   (my:minibuffer-quit))))

    (defadvice evil-visual-line (before spc-for-line-jump activate)
      (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-line-mode))
    (defadvice evil-visual-char (before spc-for-char-jump activate)
      (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))
    (defadvice evil-visual-block (before spc-for-char-jump activate)
      (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))

    ;; Switch to new splits after splitting
    (defadvice evil-window-split (after evil-window-split-then-switch activate)
      (evil-window-down 1))
    (defadvice evil-window-vsplit (after evil-window-vsplit-then-switch activate)
      (evil-window-right 1))

    ;; Add new filename symbols for ex-commands (aside from % and #)
    (defun evil-ex-replace-special-filenames (file-name)
      "Replace special symbols in FILE-NAME."
      (let ((current-fname (buffer-file-name))
            (alternate-fname (and (other-buffer)
                                  (buffer-file-name (other-buffer)))))
        (setq file-name
              ;; %:p => the project root (or current directory otherwise)
              (replace-regexp-in-string "\\(^\\|[^\\\\]\\)\\(%:p\\)"
                                        (my/project-root) file-name t t 2))
        (when current-fname
          (setq file-name
                ;; %:e => ext
                (replace-regexp-in-string "\\(^\\|[^\\\\]\\)\\(%:e\\)"
                                          (f-ext current-fname) file-name t t 2))
          (setq file-name
                ;; %:r => filename
                (replace-regexp-in-string "\\(^\\|[^\\\\]\\)\\(%:r\\)"
                                          (f-no-ext current-fname) file-name t t 2))
          (setq file-name
                ;; %:t => filename.ext
                (replace-regexp-in-string "\\(^\\|[^\\\\]\\)\\(%:t\\)"
                                          (f-base current-fname) file-name t t 2))
          (setq file-name
                ;; % => file path for current frame
                (replace-regexp-in-string "\\(^\\|[^\\\\]\\)\\(%\\)"
                                          current-fname file-name t t 2)))
        (when alternate-fname
          (setq file-name
                ;; # => file path for alternative frame
                (replace-regexp-in-string "\\(^\\|[^\\\\]\\)\\(#\\)"
                                          alternate-fname file-name t t 2)))
        (setq file-name
              (replace-regexp-in-string "\\\\\\([#%]\\)"
                                        "\\1" file-name t)))
      file-name)))


;;;; Editing plugins ;;;;;;;;;;;;;;;;;;;
(use-package anzu :init (global-anzu-mode))
(use-package expand-region :commands (er/expand-region er/contract-region))
(use-package rotate-text :commands (rotate-word-at-point rotate-region))
(use-package smart-forward)

(use-package smartparens
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)

    (setq blink-matching-paren nil)
    (setq sp-autowrap-region nil            ; let evil-surround handle this
          sp-highlight-pair-overlay nil
          sp-show-pair-delay 0
          sp-autoescape-string-quote nil)

    (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-pair "[" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-with-modes '(emacs-lisp-mode lisp-mode)
      (sp-local-pair "[" nil :post-handlers '(("|" "RET"))))

    (sp-pair "[" nil :unless '(sp-point-before-word-p sp-point-before-same-p))
    (sp-pair "(" nil :unless '(sp-point-before-word-p sp-point-before-same-p))
    (sp-pair "\"" nil :unless '(sp-point-after-word-p sp-point-before-word-p sp-point-before-same-p))
    (sp-pair "'" nil :unless '(sp-point-after-word-p sp-point-before-word-p sp-point-before-same-p))
    (sp-pair "\"" nil :unless '(sp-point-after-word-p sp-point-before-word-p sp-point-before-same-p))

    (after yasnippet
           (defadvice yas-expand (before advice-for-yas-expand activate)
             (sp-remove-active-pair-overlay)))))


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
(bind (kbd "<s-left>")      'my.move-to-bol
      (kbd "<s-right>")     'my.move-to-eol
      (kbd "<s-up>")        'smart-up
      (kbd "<s-down>")      'smart-down
      (kbd "<s-backspace>") 'my.backward-kill-to-bol-and-indent)

(add-hook! 'ido-setup-hook
           (bind ido-completion-map
             (kbd "<backspace>")  'ido-delete-backward-updir
             "\C-w"               'ido-delete-backward-word-updir))

;; Make ESC quit all the things
(bind [escape] 'keyboard-escape-quit)
(bind (list minibuffer-local-map
              minibuffer-local-ns-map
              minibuffer-local-completion-map
              minibuffer-local-must-match-map
              minibuffer-local-isearch-map) [escape] 'my:minibuffer-quit)

(bind 'god [escape] 'evil-god-state-bail)
(bind 'normal special-mode-map [escape] 'popwin:close-popup-window)
(bind isearch-mode-map "\C-w" 'isearch-abort)
;; Close help/compilation windows with escape
(bind messages-buffer-mode-map [escape] 'kill-this-buffer)
(bind special-mode-map [escape] 'popwin:close-popup-window)
;;(bind 'normal diff-mode-map
;;  [escape] 'kill-this-buffer
;;  "q" 'kill-this-buffer)
; (bind 'normal c
; scope-minor-mode-keymap
;   [escape] 'kill-this-buffer
;   "q" 'kill-this-buffer)
