(provide 'core-editor)

;;;; Editor behavior ;;;;;;;;;;;;;;;;
(setq sentence-end-double-space nil)
(setq require-final-newline t)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent nil)

;; In case of emacs 24.4
(electric-indent-mode -1)

;; Remember undo history
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist `((".*" . ,*tmp-dir-undo)))

;; Save cursor location across sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (f-expand "saveplace" *tmp-dir))

;; Save history across sessions
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; keep the home clean
      savehist-file (f-expand "savehist" *tmp-dir))
(savehist-mode 1)

;;;; Modes 'n hooks ;;;;;;;;;;;;;;;;;
(associate-mode "\\.plist$" nxml-mode)
(associate-mode "zsh\\(env\\|rc\\)?$" shell-script-mode)
(associate-mode "z\\(profile\\|login\\|logout\\)?$" shell-script-mode)
(associate-mode "zsh/" shell-script-mode)
(associate-mode "\\.applescript$" applescript-mode)
(associate-mode "Cask$" emacs-lisp-mode)
(associate-mode "\\.el\\.gz$" emacs-lisp-mode)

(add-hook 'text-mode-hook 'enable-hard-wrap)
(add-hook 'prog-mode-hook 'enable-comment-hard-wrap)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Autosave buffers on focus-out (emacs 24.4 only)
(add-hook! 'focus-out-hook (save-some-buffers t))

;;;; Evil-mode ;;;;;;;;;;;;;;;;;;;;;;;
(use-package evil
  :diminish undo-tree-mode
  :config
  (progn
    (evil-mode 1)

    (setq evil-search-module 'evil-search)
    (setq evil-magic 'very-magic)
    ;; Color-coded state cursors
    (setq evil-normal-state-cursor '("white" box))
    (setq evil-visual-state-cursor '("cyan" box))
    (setq evil-god-state-cursor '("orange" box))

    (evil-set-initial-state 'comint-mode 'insert)

    (use-package evil-ex-registers)
    (use-package evil-exchange)
    (use-package evil-indent-textobject)
    (use-package evil-numbers)
    (use-package evil-visualstar)

    (use-package evil-nerd-commenter
      :pre-load (setq evilnc-hotkey-comment-operator "g/"))

    (use-package evil-jumper
      :pre-load
      (defvar evil-jumper-file (expand-file-name "jumplist" *tmp-dir))
      :config
      (setq evil-jumper-auto-center t
            evil-jumper-auto-save-interval 3600))

    (use-package evil-space
      :init (evil-space-default-setup))
    (use-package evil-matchit
      :init (global-evil-matchit-mode 1))
    (use-package evil-surround
      :init (global-evil-surround-mode 1)
      :config
      (progn
        ;; Adds escaped delimiters to evil-surround
        (defun my.evil-surround-escaped-pair ()
          "Evil-surround function to allow escaped delimiters. e.g. \"...\""
          (let* ((input (format "\\%s" (char-to-string (read-char "\\")))))
            (cons input input)))
        (setq-default evil-surround-pairs-alist (cons '(?\\ . my.evil-surround-escaped-pair) evil-surround-pairs-alist))
        ))

    (use-package god-mode)
    (use-package evil-god-state :diminish god-local-mode)

    (defmap evil-ex-completion-map
      (kbd "C-r")           #'evil-ex-paste-from-register   ; registers in ex-mode
      (kbd "C-a")            'move-beginning-of-line
      (kbd "<s-left>")       'move-beginning-of-line
      (kbd "<s-right>")      'move-beginning-of-line
      (kbd "<s-backspace>")  'evil-delete-whole-line)

    ;; Enable half-cursor blink when using ace-jump
    (defadvice evil-ace-jump-char-mode (before evil-ace-jump-char-mode-operator-mode activate)
      (evil-half-cursor))
    (defadvice evil-ace-jump-word-mode (before evil-ace-jump-word-mode-operator-mode activate)
      (evil-half-cursor))

    ;; Exit evil-exchange mode with <Esc> (silently) -- and close
    ;; minibuffer remotely if it happens to be open
    (defadvice evil-force-normal-state (before evil-esc-quit-exchange activate)
      (shut-up (evil-exchange-cancel)
               (if (minibufferp)
                   (my:minibuffer-quit))))

    ;; Shut up beginning/end of buffer/line messages in minibuffer
    (defun my.minibuffer-bolp ()
      "Return t if (point) is truly at the beginning of the
minibuffer/evil-ex (first character after the prompt), otherwise
returns nil."
      (<= (- (point) (minibuffer-prompt-end)) 0))
    (defadvice left-char (around left-char-move-on-edges activate)
      (if (minibufferp)
          (unless (my.minibuffer-bolp) ad-do-it)
        ad-do-it))
    (defadvice right-char (around right-char-move-on-edges activate)
      (if (minibufferp)
          (unless (eolp) ad-do-it)
        ad-do-it))

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

;;;; Keymap Fixes ;;;;;;;;;;;;;;;;;;;;;;
;; This section is dedicated to keymaps that "fix" certain keys to
;; behave to be more like vim (or how I like it).

;; Restores "dumb" indentation to the tab key. This rustles a lot of
;; peoples' jimmies, apparently, but it's how I like it.
(-imap (kbd "TAB") 'my.dumb-indent)
;; Except for lisp
(imap lisp-mode-map [remap my.dumb-indent] 'indent-for-tab-command)
(imap emacs-lisp-mode-map [remap my.dumb-indent] 'indent-for-tab-command)

;; Highjacks backspace and space to:
;;   a) expand spaces between delimiters intelligently: (|) -> ( | )
;;   b) the reverse of A: ( | ) -> (|)
;;   c) And allow backspace to delete indented blocks intelligently
(-imap (kbd "SPC")                              'my.inflate-space-maybe
       [remap backward-delete-char-untabify]    'my.deflate-space-maybe
       [remap delete-backward-char]             'my.deflate-space-maybe
       [remap newline]                          'my.newline-and-indent

       ;; Smarter move-to-beginning-of-line
       [remap move-beginning-of-line]           'my.move-to-bol

       ;; Restore bash-esque keymaps in insert mode; C-w and C-a already exist
       "\C-e" 'my.move-to-eol
       "\C-u" 'my.backward-kill-to-bol-and-indent

       ;; Fix osx keymappings
       (kbd "<s-left>")      'my.move-to-bol
       (kbd "<s-right>")     'my.move-to-eol
       (kbd "<s-backspace>") 'my.backward-kill-to-bol-and-indent

       ;; Fixes delete
       (kbd "<kp-delete>")   'delete-char

       ;; Textmate-esque insert-line before/after
       (kbd "<s-return>")    'evil-open-below
       (kbd "<S-s-return>")  'evil-open-above)

(add-hook! 'ido-setup-hook
           (defmap ido-completion-map
             (kbd "<backspace>")  'ido-delete-backward-updir
             "\C-w"               'ido-delete-backward-word-updir))

;; Make ESC quit all the things
(global-set-key [escape] 'keyboard-escape-quit)
(mapc (lambda (map)
    (defmap map [escape] 'my:minibuffer-quit))
      (list minibuffer-local-map
            minibuffer-local-ns-map
            minibuffer-local-completion-map
            minibuffer-local-must-match-map
            minibuffer-local-isearch-map))
(defmap isearch-mode-map [escape] 'isearch-abort)
(defmap evil-god-state-map [escape] 'evil-god-state-bail)
(defmap evil-ex-search-keymap [escape] 'evil-ex-search-exit)
;; Close help/compilation windows with escape
(defmap help-mode-map [escape] 'kill-buffer-and-window)
(defmap compilation-mode-map [escape] 'kill-buffer-and-window)
(emap debugger-mode-map [remap evil-exit-emacs-state] 'kill-buffer-and-window)

;;;; Editing plugins ;;;;;;;;;;;;;;;;;;;
(use-package expand-region)

(use-package smartparens
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)

    (setq blink-matching-paren nil)
    (setq sp-autowrap-region nil            ; let evil-surround handle this
          sp-highlight-pair-overlay nil
          sp-show-pair-delay 0
          sp-autoescape-string-quote t)

    (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-pair "[" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair 'emacs-lisp-mode "[" nil :post-handlers '(("|" "RET")))

    (sp-pair "\"" nil :unless '(sp-point-after-word-p sp-point-before-word-p))
    (sp-pair "'" nil :unless '(sp-point-after-word-p sp-point-before-word-p))

    (after yasnippet
           (defadvice yas-expand (before advice-for-yas-expand activate)
             (sp-remove-active-pair-overlay)))))

(use-package anzu
  :diminish anzu-mode
  :init
  (global-anzu-mode))

;;;; Utility plugins ;;;;;;;;;;;;;;;;;;
(use-package key-chord
  :init
  (progn (key-chord-mode 1)
         (setq key-chord-two-keys-delay 0.5)))

(use-package smex
  :commands (smex smex-major-mode-commands)
  :config
  (progn (smex-initialize)
         ;; Hook up smex to auto-update, rather than update on every run
         (defun smex-update-after-load (unused)
           (when (boundp 'smex-cache) (smex-update)))
         (add-hook 'after-load-functions 'smex-update-after-load)))

(use-package recentf
  :init
  (progn (recentf-mode 1)
         (setq recentf-max-menu-items 0
               recentf-max-saved-items 100
               recentf-auto-cleanup 'never
               recentf-exclude '("/tmp/" "/ssh:" "\\.ido\\.last\\'" "\\.revive\\'"))))

(use-package re-builder
  :defer t
  :config
  (progn (setq reb-re-syntax 'string)
         (defadvice evil-force-normal-state (before evil-esc-quit-reb-mode activate)
           (when (eq 'reb-mode major-mode)
             (if reb-subexp-mode
                 (reb-quit-subexp-mode)
               (reb-quit))))))
