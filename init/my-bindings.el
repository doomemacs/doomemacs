;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global keymaps                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind "A-x"      'smex
      "A-X"      'smex-major-mode-commands
      "A-M-x"    'helm-M-x
      "A-;"      'eval-expression
      "C-`"      'popwin:toggle-popup-window
      "M-="      'text-scale-increase
      "M--"      'text-scale-decrease
      "M-w"      'evil-window-delete
      "M-/"      'evil-commentary-line
      "M-b"      'my:build)

(bind 'motion
      ;; Faster scrolling
      "M-j"  "6j"
      "M-k"  "6k"
      "M-r"  'my:eval-region

      'normal
      "M-o"  'ido-find-file
      "M-O"  'my-ido-find-project-file
      "M-d"  'dash-at-point
      "M-R"  'my:eval-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local keymaps                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-prefix-command 'my-leader-map)
(define-prefix-command 'my-localleader-map)

(bind '(normal motion visual) ";" 'evil-ex)

;; <leader>
(bind my-leader-map
      ","   'helm-projectile-switch-to-buffer
      "."   'ido-find-file
      ">"   'my-ido-find-project-file
      "/"   'helm-projectile-find-file
      ";"   'helm-semantic-or-imenu
      "<"   'helm-mini
      "M"   'helm-projectile-recentf ; recent PROJECT files
      "]"   'helm-etags-select
      "a"   'helm-projectile-find-other-file
      "E"   'my:init-files
      "g"   'git-gutter+-show-hunk
      "h"   'helm-apropos
      "m"   'helm-recentf
      "p"   'helm-projectile-switch-project
      "r"   'emr-show-refactor-menu   ; init-dev.el
      "qq"  'evil-save-and-quit
      "QQ"  'evil-quit-all

      "oo"  'my-open-with
      "of"  (λ (my-open-with "Finder.app" default-directory))
      "oF"  (λ (my-open-with "Finder.app" (project-root)))
      "ou"  (λ (my-open-with "Transmit"))
      "oU"  (λ (my-open-with "Transmit" default-directory))
      "ol"  (λ (my-open-with "LaunchBar"))
      "oL"  (λ (my-open-with "LaunchBar" default-directory))
      ;; tmux: cd (default-directory)
      "ot"  (λ (my:tmux-chdir nil t))
      ;; tmux: cd [project root]
      "oT"  'my:tmux-chdir)

;; <localleader>
(bind my-localleader-map
      "\\"  'neotree-toggle
      ";"   'linum-mode
      "="   'toggle-transparency
      "E"   'evil-emacs-state

      "]"   'next-buffer
      "["   'previous-buffer

      "g"   'git-gutter+-show-hunk
      "e"   (λ (flycheck-buffer) (flycheck-list-errors))
      "p"   'helm-show-kill-ring
      "b"   'helm-projectile-switch-to-buffer
      "w"   'helm-wg)


(bind 'normal
      ","    'my-leader-map
      "\\"   'my-localleader-map

      ;; behave  like D and C; yank to end of line
      "Y"       (λ (evil-yank (point) (point-at-eol)))
      "zx"     'my-kill-real-buffer
      "ZX"     'bury-buffer
      "]b"     'my-next-real-buffer
      "[b"     'my-previous-real-buffer
      "]w"     'wg-switch-to-workgroup-right
      "[w"     'wg-switch-to-workgroup-left

      ;; Increment/decrement number under cursor
      "g="       'evil-numbers/inc-at-pt
      "g-"       'evil-numbers/dec-at-pt
      "gR"       'my:eval-buffer  ; init-dev.el

      'visual
      ", ="   'align-regexp

      ;; vnoremap < <gv
      "<"    (λ (evil-shift-left (region-beginning) (region-end))
                (evil-normal-state)
                (evil-visual-restore))
      ;; vnoremap > >gv
      ">"    (λ (evil-shift-right (region-beginning) (region-end))
                (evil-normal-state)
                (evil-visual-restore))

      'normal "X" 'evil-exchange

      'motion
      "]g"   'git-gutter+-next-hunk
      "[g"   'git-gutter+-previous-hunk

      "]e"   (λ (call-interactively (if flycheck-mode 'flycheck-next-error 'next-error)))
      "[e"   (λ (call-interactively (if flycheck-mode 'flycheck-previous-error 'previous-error)))

      "]\\"  'er/expand-region
      "[\\"  'er/contract-region

      "gl"   (λ (linum-mode 1) (evil-ex "") (linum-mode -1))
      "gx"   'my-scratch-buffer    ; send to scratch buffer
      "gr"   'my:eval-region       ; init-dev.el

      'insert
      "<A-backspace>" 'evil-delete-backward-word
      "<A-delete>"    (λ (evil-forward-word) (evil-delete-backward-word))

      ;; Newline magic
      "<backspace>"   'backward-delete-char-untabify
      "<M-backspace>" 'my.backward-kill-to-bol-and-indent
      "<C-return>"    'evil-ret-and-indent

      ;; Textmate-esque indent shift left/right
      "M-["           (kbd "C-o m l C-o I DEL C-o ` l")
      "M-]"           (λ (evil-shift-right (point-at-bol) (point-at-eol)))
      "<backtab>"     (kbd "M-[")

      ;; Easy escape from insert mode (more responsive than using key-chord-define)
      "j"      'my--maybe-exit-insert-mode
      "C-g"    'evil-normal-state

      ;; Rotate-text (see elisp/rotate-text.el)
      'normal "!" 'rotate-word-at-point
      'visual "!" 'rotate-region

      'emacs
      [escape]  'evil-normal-state)

;; Enable TAB to do matchit
(bind 'motion evil-matchit-mode-map [tab] 'evilmi-jump-items)

(bind evil-window-map
      ;; winner-mode: window layout undo/redo (see init-core.el)
      "u"     'winner-undo
      "C-u"   'winner-undo
      "C-r"   'winner-redo)

;; Peek at file from dired
(bind dired-mode-map "o" (λ (popwin:find-file (dired-get-file-for-visit))))

(after "help-mode"
  (bind 'normal help-mode-map
        "]]" 'help-go-forward
        "[[" 'help-go-back))

(bind '(insert normal)
      ;; Textmate-esque insert-line before/after
      (kbd "<M-return>")    'evil-open-below
      (kbd "<S-M-return>")  'evil-open-above)

(when is-mac
  ;; Restore text nav keys
  (bind "<A-left>" 'backward-word
        "<A-right>" 'forward-word
        "<M-backspace>" 'my.backward-kill-to-bol-and-indent
        "M-a" 'mark-whole-buffer
        "M-c" 'evil-yank
        "M-s" 'save-buffer
        "M-v" 'yank))

;; Fix osx keymappings and then some
(use-package smart-forward
  :config
  (bind 'insert
        "<M-left>"   'my.move-to-bol
        "<M-right>"  'my.move-to-eol
        "<M-up>"     'beginning-of-buffer
        "<M-down>"   'end-of-buffer
        "<A-up>"     'smart-up
        "<A-down>"   'smart-down))

;; Line selection via linum
(bind "<left-margin> <down-mouse-1>"    'my-select-linum
      "<left-margin> <mouse-1>"         'my-select-linum
      "<left-margin> <drag-mouse-1>"    'my-select-linum
      "<left-margin> <double-mouse-1>"  'my-select-block)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap fixes                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This section is dedicated to keymaps that "fix" certain keys so
;; that they behave more like vim (or how I like it).

;; Restores "dumb" indentation to the tab key. This rustles a lot of
;; peoples' jimmies, apparently, but it's how I like it.
(bind 'insert "<tab>" 'my.dumb-indent)
(bind 'insert "<A-tab>" 'indent-for-tab-command)

;; Except for lisp
(bind 'insert lisp-mode-map        [remap my.dumb-indent] 'indent-for-tab-command)
(bind 'insert emacs-lisp-mode-map  [remap my.dumb-indent] 'indent-for-tab-command)

;; Highjacks space/backspace to:
;;   a) delete spaces on either side of the cursor, if present ( | ) -> (|)
;;   b) allow backspace to delete space-indented blocks intelligently
;;   c) and not do any of this magic when inside a string
(bind 'insert
      "SPC"                                  'my.inflate-space-maybe
      [remap backward-delete-char-untabify]  'my.deflate-space-maybe
      [remap newline]                        'my.newline-and-indent

      ;; Smarter move-to-beginning-of-line
      [remap move-beginning-of-line]         'my.move-to-bol

      ;; Restore bash-esque keymaps in insert mode; C-w and C-a already exist
      "C-e" 'my.move-to-eol
      "C-u" 'my.backward-kill-to-bol-and-indent

      ;; Fixes delete
      (kbd "<kp-delete>")   'delete-char)

;; Make ESC quit all the things
(dolist (map (list minibuffer-local-map
                   minibuffer-local-ns-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-isearch-map))
  (bind map "<escape>" 'keyboard-escape-quit))

(dolist (map (list evil-ex-search-keymap minibuffer-local-map))
  (bind map "\C-w" 'evil-delete-backward-word))

(global-unset-key (kbd "<drag-mouse-1>"))

(provide 'my-bindings)
;;; my-bindings.el ends here
