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

(when is-mac
  ;; Restore text nav keys
  (bind "<A-left>" 'backward-word
        "<A-right>" 'forward-word
        "<M-backspace>" 'my.backward-kill-to-bol-and-indent
        "A-SPC" 'just-one-space
        "M-a" 'mark-whole-buffer
        "M-c" 'evil-yank
        "M-s" 'save-buffer
        "M-v" 'yank))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local keymaps                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-prefix-command 'my-leader-map)
(define-prefix-command 'my-localleader-map)

(bind '(normal motion visual) ";" 'evil-ex)

;; <leader>
(bind my-leader-map
      ","   (λ (if (projectile-project-p) (helm-projectile-switch-to-buffer) (helm-buffers-list)))
      "<"   'helm-buffers-list
      "."   'ido-find-file
      ">"   (λ (let ((default-directory (project-root))) (ido-find-file)))
      "/"   'helm-projectile-find-file
      ";"   'helm-semantic-or-imenu
      "]"   'helm-etags-select
      "a"   'helm-projectile-find-other-file
      "E"   'my:init-files
      "g"   'git-gutter+-show-hunk
      "h"   'helm-apropos
      "m"   'helm-recentf
      "M"   'helm-projectile-recentf ; recent PROJECT files
      "p"   'helm-projectile-switch-project
      "r"   'emr-show-refactor-menu   ; init-dev.el
      "qq"  'evil-save-and-quit
      "QQ"  (λ (my:kill-buffers t) (evil-quit-all))

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
      "\\"  'my-neotree-toggle
      "."   'my-neotree-find
      ";"   'linum-mode
      "="   'toggle-transparency
      "E"   'evil-emacs-state

      "]"   'next-buffer
      "["   'previous-buffer

      "g"   'git-gutter+-show-hunk
      "e"   (λ (flycheck-buffer) (flycheck-list-errors))
      "p"   'helm-show-kill-ring
      "b"   'helm-bookmarks
      "w"   'helm-wg)


(bind 'normal
      ","      'my-leader-map
      "\\"     'my-localleader-map

      ;; behave  like D and C; yank to end of line
      "Y"    (λ (evil-yank (point) (point-at-eol)))
      "zx"   'my-kill-real-buffer
      "ZX"   'bury-buffer
      "]b"   'my-next-real-buffer
      "[b"   'my-previous-real-buffer
      "]w"   'wg-switch-to-workgroup-right
      "[w"   'wg-switch-to-workgroup-left

      ;; Increment/decrement number under cursor
      "g="   'evil-numbers/inc-at-pt
      "g-"   'evil-numbers/dec-at-pt
      "gR"   'my:eval-buffer  ; init-dev.el
      "gc"   'evil-commentary
      "gy"   'evil-commentary-yank

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
      "%"    'evilmi-jump-items
      [tab]  'evilmi-jump-items    ; alias for %

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

      ;; Company-mode
      "C-SPC"     'company-complete-common
      "C-x C-k"   'company-dictionary
      "C-x C-f"   'company-files
      "C-x C-]"   'company-etags
      "C-x s"     'company-ispell
      "C-x C-s"   'company-yasnippet
      "C-x C-o"   'company-semantic
      "C-x C-n"   'company-dabbrev-code
      "C-x C-p"   (λ (let ((company-selection-wrap-around t))
                       (call-interactively 'company-dabbrev-code)
                       (company-select-previous-or-abort)))

      'operator
      "s"   'evil-surround-edit
      "S"   'evil-Surround-edit

      'visual
      "z"   'evil-snipe-s
      "Z"   'evil-snipe-S

      "S"   'evil-surround-region

      ;; Rotate-text (see elisp/rotate-text.el)
      'normal "!" 'rotate-word-at-point
      'visual "!" 'rotate-region

      'emacs
      [escape]  'evil-normal-state)

(bind evil-window-map
      ;; winner-mode: window layout undo/redo (see core.el)
      "u"     'winner-undo
      "C-u"   'winner-undo
      "C-r"   'winner-redo)

;; Real go-to-definition for elisp
(bind 'motion emacs-lisp-mode-map
      "gd" (λ (let ((func (function-called-at-point)))
                (if func (find-function func))))
      "gD" (λ (let ((func (function-called-at-point)))
                (if func (find-function-other-window func)))))

;; Peek at file from dired
;; (bind dired-mode-map "o" (λ (popwin:find-file (dired-get-file-for-visit))))

(after "help-mode"
  (bind 'normal help-mode-map
        "]]" 'help-go-forward
        "[[" 'help-go-back))

(bind '(insert normal)
      ;; Textmate-esque insert-line before/after
      (kbd "<M-return>")    'evil-open-below
      (kbd "<S-M-return>")  'evil-open-above)

;; Fix osx keymappings and then some
(bind 'insert
      "<M-left>"   'my.move-to-bol
      "<M-right>"  'my.move-to-eol
      "<M-up>"     'beginning-of-buffer
      "<M-down>"   'end-of-buffer
      "<A-up>"     'smart-up
      "<A-down>"   'smart-down)

;; Line selection via linum
(bind "<left-margin> <down-mouse-1>"    'my-select-linum
      "<left-margin> <mouse-1>"         'my-select-linum
      "<left-margin> <drag-mouse-1>"    'my-select-linum
      "<left-margin> <double-mouse-1>"  'my-select-block)

(bind 'visual
      "*" 'evil-visualstar/begin-search-forward
      "#" 'evil-visualstar/begin-search-backward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap fixes                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This section is dedicated to keymaps that "fix" certain keys so
;; that they behave more like vim (or how I like it).

;; Restores "dumb" indentation to the tab key. This rustles a lot of
;; peoples' jimmies, apparently, but it's how I like it.
(bind 'insert "<tab>" 'my.dumb-indent)
(bind 'insert "<A-tab>" 'indent-for-tab-command)

;; No dumb-tab for lisp
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
  (bind map [escape] 'my--minibuffer-quit))
(bind 'emacs [escape] 'my--minibuffer-quit)

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

(dolist (map (list evil-ex-search-keymap minibuffer-local-map))
  (bind map "\C-w" 'evil-delete-backward-word))

(global-unset-key (kbd "<drag-mouse-1>"))

(provide 'my-bindings)
;;; my-bindings.el ends here
