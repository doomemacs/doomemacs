(provide 'my-bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global keymaps                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind (kbd "M-x")      'smex
      (kbd "M-X")      'smex-major-mode-commands
      (kbd "C-;")      'eval-expression
      ;; (kbd "C-`")      'popwin:toggle-eshell-window
      (kbd "C-~")      'popwin:toggle-popup-window

      (kbd "s-=")      'text-scale-increase
      (kbd "s--")      'text-scale-decrease
      (kbd "s-w")      'evil-window-delete
      (kbd "s-/")      'evilnc-comment-or-uncomment-lines)

;; Faster scrolling
(bind '(normal visual) my-mode-map
      (kbd "s-j")      "6j"
      (kbd "s-k")      "6k"
      (kbd "s-b")      'my:build
      (kbd "s-f")      'helm-swoop
      (kbd "s-F")      'helm-do-ag)

(bind 'normal my-mode-map
      (kbd "s-r")      'my-run-code-buffer
      (kbd "s-R")      'my-switch-to-repl
      (kbd "s-t")      'helm-projectile-find-file
      (kbd "s-T")      'helm-semantic-or-imenu
      (kbd "s-p")      'helm-projectile-switch-project
      (kbd "s-P")      'persp-switch
      (kbd "s-m")      ",m"
      (kbd "s-M")      ",M"
      (kbd "s-o")      'ido-find-file
      (kbd "s-d")      'dash-at-point)

(bind 'visual my-mode-map
      (kbd "s-r")      'my-run-code-region
      (kbd "s-R")      'my-send-region-to-repl)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local keymaps                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind '(normal visual) my-mode-map
      "\\"    'evil-execute-in-god-state ; localleader
      ;; ";"     'evil-ex ; Remap ; to : - SPC and shift-SPC replace ; and ,
      "X"     'evil-exchange

      "g SPC" (λ (call-interactively
                  (if (evil-visual-line-state-p)
                      'evil-ace-jump-line-mode
                    'evil-ace-jump-char-mode)))
      "g w"   'evil-ace-jump-word-mode  ; overwrites evil-fill
      "g l"   'evil-ace-jump-line-mode
      "g s"   'evil-ace-jump-two-chars-mode
      "g t"   'ace-window
      "g T"   (λ (ace-window 4))

      "] e"   'next-error
      "[ e"   'previous-error
      "] g"   'git-gutter+-next-hunk
      "[ g"   'git-gutter+-previous-hunk

      "] \\"  'er/expand-region
      "[ \\"  'er/contract-region)

(bind 'normal my-mode-map
      ", a"   'helm-projectile-find-other-file
      ", e"   'ido-find-file
      ", E"   'my:init-files
      ", f"   'helm-projectile-find-file-dwim
      ", g"   'git-gutter+-show-hunk
      ", h"   'helm-apropos
      ", m"   'helm-recentf
      ", M"   'helm-projectile-recentf ; recent PROJECT files
      ", p"   'helm-projectile-switch-project
      ", y"   'helm-show-kill-ring
      ", ;"   'helm-semantic-or-imenu
      ", ,"   'helm-projectile-switch-to-buffer
      ", <"   'helm-mini
      ", ]"   'helm-etags-select
      ", /"   'helm-projectile-find-file
      ", ."   'helm-resume)

(bind 'god my-mode-map
      ;; <localleader>
      "\\"   'neotree-toggle
      ":"    'linum-mode
      "="    'toggle-transparency

      "]"    'next-buffer
      "["    'previous-buffer

      "o f"  'my-send-dir-to-finder
      "o u"  'my-send-to-transmit
      "o l"  'my-send-to-launchbar
      "o L"  'my-send-dir-to-launchbar

      ;; tmux: cd (default-directory)
      "o t"  (λ (my:tmux-chdir nil t))
      ;; tmux: cd [project root]
      "o T"  'my:tmux-chdir)

(bind 'normal my-mode-map
      ;; behave  like D and C; yank to end of line
      "Y"    (λ (evil-yank (point) (point-at-eol)))

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

(bind 'insert my-mode-map
      "<M-kp-delete>" (λ (evil-forward-word) (evil-delete-backward-word))

      ;; Newline magic
      "<backspace>"   'backward-delete-char-untabify
      "<S-backspace>" 'backward-delete-char
      "<C-return>"    'evil-ret-and-indent
      "<M-return>"    (kbd "<return> DEL") ; newline and dedent

      ;; Textmate-esque indent shift left/right
      "s-["           (kbd "C-o m l C-o I DEL C-o ` l")
      "s-]"           (λ (evil-shift-right (point-at-bol) (point-at-eol)))
      "<backtab>"     (kbd "s-["))

;; (bind 'emacs
;;       ;; Preserve buffer-movement in emacs mode
;;       "C-j"        'evil-next-line
;;       "C-k"        'evil-previous-line

;;       "C-w h" 'evil-window-left
;;       "C-w l" 'evil-window-right
;;       "C-w j" 'evil-window-down
;;       "C-w k" 'evil-window-up)

;; Rotate-text (see elisp/rotate-text.el)
(bind 'normal my-mode-map "!" 'rotate-word-at-point)
(bind 'visual my-mode-map "!" 'rotate-region)

;; Easy escape from insert mode
;; (ibind "jk" 'evil-normal-state)

;; Enable TAB to do matchit
(bind '(normal visual) evil-matchit-mode-map [tab] 'evilmi-jump-items)

;; Additional operators
(bind 'normal my-mode-map "g r" 'my:run-code)            ; code eval
(bind 'normal my-mode-map "g R" 'my:send-region-to-repl) ; eval in repl
(bind 'normal my-mode-map "g x" 'my-scratch-buffer)      ; send to scratch buffer


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
