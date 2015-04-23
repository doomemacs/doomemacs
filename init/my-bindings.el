;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global keymaps                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind (kbd "≈")        'smex
      (kbd "˛")        'smex-major-mode-commands
      (kbd "C-;")      'eval-expression
      (kbd "C-`")      'popwin:toggle-popup-window

      (kbd "M-=")      'text-scale-increase
      (kbd "M--")      'text-scale-decrease
      (kbd "M-w")      'evil-window-delete
      (kbd "M-/")      'evilnc-comment-or-uncomment-lines
      (kbd "M-s")      'save-buffer)

;; Faster scrolling
(bind 'motion my-mode-map
      (kbd "M-j")      "6j"
      (kbd "M-k")      "6k")

(bind 'normal my-mode-map
      (kbd "M-o")      'ido-find-file
      (kbd "M-d")      'dash-at-point)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local keymaps                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind '(normal visual) my-mode-map
      ";"     'evil-ex
      "X"     'evil-exchange

      "g l"   (λ (linum-mode 1) (evil-ex "") (linum-mode -1))

      "] e"   'next-error
      "[ e"   'previous-error
      "] g"   'git-gutter+-next-hunk
      "[ g"   'git-gutter+-previous-hunk

      "] \\"  'er/expand-region
      "[ \\"  'er/contract-region)

(bind 'normal my-mode-map
      ;; <leader>
      ", ,"   'helm-projectile-switch-to-buffer
      ", ."   'helm-resume
      ", /"   'helm-projectile-find-file
      ", ;"   'helm-semantic-or-imenu
      ", <"   'helm-mini
      ", E"   'my:init-files
      ", M"   'helm-projectile-recentf ; recent PROJECT files
      ", ]"   'helm-etags-select
      ", a"   'helm-projectile-find-other-file
      ", e"   'ido-find-file
      ", f"   'helm-projectile-find-file-dwim
      ", g"   'git-gutter+-show-hunk
      ", h"   'helm-apropos
      ", m"   'helm-recentf
      ", p"   'helm-projectile-switch-project
      ", y"   'helm-show-kill-ring

      ;; <localleader>
      "\\ \\"   'neotree-toggle
      "\\ ;"    'linum-mode
      "\\ ="    'toggle-transparency
      "\\ e"    'evil-emacs-state

      "\\ ]"    'next-buffer
      "\\ ["    'previous-buffer

      "\\ o f"  (λ (my-send-dir-to-finder default-directory))
      "\\ o F"  'my-send-dir-to-finder
      "\\ o u"  (λ (my-send-to-transmit buffer-file-name))
      "\\ o U"  'my-send-to-transmit
      "\\ o l"  (λ (my-send-to-launchbar buffer-file-name))
      "\\ o L"  'my-send-to-launchbar

      ;; tmux: cd (default-directory)
      "\\ o t"  (λ (ex:tmux-chdir nil t))
      ;; tmux: cd [project root]
      "\\ o T"  'ex:tmux-chdir

      ;; behave  like D and C; yank to end of line
      "Y"     (λ (evil-yank (point) (point-at-eol)))

      "z x"       'kill-this-buffer
      "Z X"       'bury-buffer

      "] b"       'next-buffer
      "[ b"       'previous-buffer
      "] p"       'persp-next
      "[ p"       'persp-prev

      ;; winner-mode: window layout undo/redo (see init-core.el)
      "C-w u"     'winner-undo
      "C-w C-u"   'winner-undo
      "C-w C-r"   'winner-redo

      ;; Increment/decrement number under cursor
      "C-="       'evil-numbers/inc-at-pt
      "C--"       'evil-numbers/dec-at-pt)

(bind 'visual my-mode-map
      ", ="   'align-regexp

      ;; vnoremap < <gv
      "<"    (λ (evil-shift-left (region-beginning) (region-end))
                (evil-normal-state)
                (evil-visual-restore))
      ;; vnoremap > >gv
      ">"    (λ (evil-shift-right (region-beginning) (region-end))
                (evil-normal-state)
                (evil-visual-restore)))

(bind 'emacs [escape] 'evil-normal-state)

(bind 'insert my-mode-map
      "<A-backspace>" 'evil-delete-backward-word
      "<A-delete>"    (λ (evil-forward-word) (evil-delete-backward-word))

      ;; Newline magic
      "<backspace>"   'backward-delete-char-untabify
      "<M-backspace>" 'my.backward-kill-to-bol-and-indent
      "<C-return>"    'evil-ret-and-indent

      ;; Textmate-esque indent shift left/right
      "M-["           (kbd "C-o m l C-o I DEL C-o ` l")
      "M-]"           (λ (evil-shift-right (point-at-bol) (point-at-eol)))
      "<backtab>"     (kbd "M-["))

;; Enable TAB to do matchit
(bind '(normal visual) evil-matchit-mode-map [tab] 'evilmi-jump-items)

;; Rotate-text (see elisp/rotate-text.el)
(bind 'normal my-mode-map "!" 'rotate-word-at-point)
(bind 'visual my-mode-map "!" 'rotate-region)

;; Additional operators
(bind 'motion my-mode-map "g x" 'my-scratch-buffer) ; send to scratch buffer

;; Easy escape from insert mode (more responsive than using key-chord-define)
(bind 'insert "j" 'my--maybe-exit-insert-mode)

(bind 'insert "C-g" 'evil-normal-state)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plugin/mode keymaps                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Peek at file from dired
(bind dired-mode-map "o" (λ (popwin:find-file (dired-get-file-for-visit))))

;; Evil registers ;;;;;;;;;;;;;;;;;;;;;;

(after "help-mode"
  (bind 'normal help-mode-map
        "]]" 'help-go-forward
        "[[" 'help-go-back))

(evil-make-overriding-map my-mode-map nil)


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
      "\C-e" 'my.move-to-eol
      "\C-u" 'my.backward-kill-to-bol-and-indent

      ;; Fixes delete
      (kbd "<kp-delete>")   'delete-char)

(bind '(insert normal)
      ;; Textmate-esque insert-line before/after
      (kbd "<M-return>")    'evil-open-below
      (kbd "<S-M-return>")  'evil-open-above)

;; Fix osx keymappings and then some
(use-package smart-forward
  :config
  (bind 'insert
        (kbd "<M-left>")      'my.move-to-bol
        (kbd "<M-right>")     'my.move-to-eol
        (kbd "<M-backspace>") 'my.backward-kill-to-bol-and-indent
        ;; (kbd "<M-up>")        'beginning-of-buffer
        (kbd "<M-up>")        'smart-up
        (kbd "<M-down>")      'smart-down))



(provide 'my-bindings)
;;; my-bindings.el ends here
