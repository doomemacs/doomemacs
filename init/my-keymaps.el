(provide 'my-keymaps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global keymaps                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind (kbd "M-x") 'smex
      (kbd "M-X") 'smex-major-mode-commands
      (kbd "C-;") 'eval-expression
      (kbd "C-`") 'popwin:popup-last-buffer

      (kbd "s-=")     'text-scale-increase
      (kbd "s--")     'text-scale-decrease
      (kbd "s-w")     'evil-window-delete
      (kbd "s-/")     'evilnc-comment-or-uncomment-lines
      (kbd "s-<f12>") 'toggle-frame-fullscreen)

;; Faster scrolling
(bind '(normal visual)
      (kbd "s-j")   "7j"
      (kbd "s-k")   "7k"
      (kbd "s-r")   'my:run-code-buffer
      (kbd "s-R")   'my:switch-to-repl)

(bind 'normal  (kbd "s-t")   'projectile-find-file
      (kbd "s-T")   'projectile-find-tag
      (kbd "s-p")   'projectile-switch-project
      (kbd "s-P")   'persp-switch
      (kbd "s-f")   'projectile-ag
      (kbd "s-S-f") 'helm-do-ag
      (kbd "s-m")   ",m"
      (kbd "s-M")   ",M"
      (kbd "s-o")   'ido-find-file
      (kbd "s-d")   'dash-at-point)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local keymaps                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind 'god
      ;; <localleader>
      ":"    'linum-mode
      "\\"   'neotree-toggle
      "="    'toggle-transparency

      "of"   'my:send-dir-to-finder
      "ou"   'my:send-to-transmit
      "ol"   'my:send-to-launchbar
      "oL"   'my:send-dir-to-launchbar

      ;; tmux: cd (default-directory)
      "ot"   (λ (ex:tmux-chdir nil t))
      ;; tmux: cd [project root]
      "oT"   'ex:tmux-chdir)

(bind '(normal visual)
      ";"    'evil-ex ; Remap ; to : - SPC and shift-SPC replace ; and ,
      "\\"   'evil-execute-in-god-state ; localleader
      "X"    'evil-exchange

      "gc"   'evil-ace-jump-char-mode
      "gw"   'evil-ace-jump-word-mode  ; overwrites evil-fill
      "gl"   'evil-ace-jump-line-mode
      "gt"   'ace-window
      "gT"   (λ (ace-window 4))

      "]e"   'next-error
      "[e"   'previous-error
      "]g"   'git-gutter+-next-hunk
      "[g"   'git-gutter+-previous-hunk

      "]\\"  'er/expand-region
      "[\\"  'er/contract-region)

(bind 'normal
      ",r"   'my:run-code-buffer
      ",R"   'my:switch-to-repl
      ",a"   'helm-projectile-find-other-file
      ",e"   'ido-find-file
      ",E"   'ex:init-files
      ",m"   'helm-recentf
      ",M"   'helm-projectile-recentf ; recent PROJECT files
      ",p"   'helm-projectile-switch-project
      ",g"   'git-gutter+-show-hunk
      ",;"   'helm-imenu
      ",,"   'helm-projectile-switch-to-buffer
      ",<"   'helm-buffers-list
      ",]"   'helm-etags-select
      ",/"   'helm-projectile-find-file
      ",."   'helm-projectile-find-file-dwim

      ;; Moving rows rather than lines (in case of wrapping)
      "j"    'evil-next-visual-line
      "k"    'evil-previous-visual-line

      ;; behave  like D and C; yank to end of line
      "Y"    (λ (evil-yank (point) (point-at-eol)))

      "zz"   'kill-this-buffer
      "zx"   'bury-buffer

      "]b"   'next-buffer
      "[b"   'previous-buffer
      "]p"   'persp-next
      "[p"   'persp-prev

      ;; winner-mode: window layout undo/redo (see init-core.el)
      (kbd "C-w u")     'winner-undo
      (kbd "C-w C-r")   'winner-redo

      ;; Increment/decrement number under cursor
      (kbd "C--")       'evil-numbers/inc-at-pt
      (kbd "C-+")       'evil-numbers/dec-at-pt)

(bind 'visual
      ",="   'align-regexp
      ",r"   'my:run-code-region
      ",R"   'my:send-region-to-repl

      ;; vnoremap < <gv
      "<"    (λ (evil-shift-left (region-beginning) (region-end))
                (evil-normal-state)
                (evil-visual-restore))
      ;; vnoremap > >gv
      ">"    (λ (evil-shift-right (region-beginning) (region-end))
                (evil-normal-state)
                (evil-visual-restore)))

(bind 'insert
      ;; Join lines from insert mode
      (kbd "<M-kp-delete>") (λ (evil-forward-word) (evil-delete-backward-word))

      ;; Newline magic
      (kbd "<backspace>")   'backward-delete-char-untabify
      (kbd "<S-backspace>") 'backward-delete-char
      (kbd "<C-return>")    'evil-ret-and-indent
      (kbd "<M-return>")    (kbd "<return> DEL") ; newline and dedent

      ;; Textmate-esque indent shift left/right
      (kbd "s-[")           (kbd "C-o m l C-o I DEL C-o ` l")
      (kbd "s-]")           (λ (evil-shift-right (point-at-bol) (point-at-eol)))
      (kbd "<backtab>")     (kbd "s-["))

(bind 'emacs
      ;; Preserve buffer-movement in emacs mode
      "\C-j"        'evil-next-line
      "\C-k"        'evil-previous-line

      (kbd "C-w h") 'evil-window-left
      (kbd "C-w l") 'evil-window-right
      (kbd "C-w j") 'evil-window-down
      (kbd "C-w k") 'evil-window-up)

;; Rotate-text (see elisp/rotate-text.el)
(bind 'normal "!" 'rotate-word-at-point)
(bind 'visual "!" 'rotate-region)

;; Easy escape from insert mode
(ibind "jj" 'evil-normal-state)

;; Enable TAB to do matchit
(bind '(normal visual) evil-matchit-mode-map [tab] 'evilmi-jump-items)

;; Additional operators
(bind '(normal motion) "gr" 'ex:run-code)            ; code eval
(bind '(normal motion) "gR" 'ex:send-region-to-repl) ; eval in repl
(bind '(normal motion) "gx" 'ex:scratch-buffer)      ; send to scratch buffer


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plugin/mode keymaps                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Real go-to-definition for elisp
(bind 'motion emacs-lisp-mode-map "gd"
      (λ (let ((func (function-called-at-point)))
           (if func (find-function func)))))
(bind 'motion emacs-lisp-mode-map "gD"
      (λ (let ((func (function-called-at-point)))
           (if func (find-function-other-window func)))))

;; Peek at file from dired
(bind dired-mode-map "o" (λ (popwin:find-file (dired-get-file-for-visit))))

;; Evil registers ;;;;;;;;;;;;;;;;;;;;;;
(bind evil-ex-completion-map
      (kbd "C-r")           #'evil-ex-paste-from-register   ; registers in ex-mode
      (kbd "C-a")            'move-beginning-of-line
      (kbd "<s-left>")       'move-beginning-of-line
      (kbd "<s-right>")      'move-beginning-of-line
      (kbd "<s-backspace>")  'evil-delete-whole-line)
;; Quickly close/kill the command window
(bind 'normal evil-command-window-mode-map
      [escape]  'kill-buffer-and-window
      "q"       'kill-buffer-and-window)

;; Make C-g work like <esc>
(bind '(normal visual insert) (kbd "C-g") 'evil-normal-state)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ex Commands                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcmd "msg"         'popwin:messages)
(defcmd "recompile"   'ex:byte-compile-all)
(defcmd "n[otes]"     'ex:notes)
(defcmd "ini"         'ex:init-files)
(defcmd "rec[ent]"    'ex:recent-files)
(defcmd "snip[pets]"  'ex:snippets)
(defcmd "retab"       'ex:retab)
(defcmd "ag"          'ex:ag-search)
(defcmd "agr"         'ex:ag-regex-search)
(defcmd "x"           'ex:scratch-buffer)
(defcmd "X"           'ex:org-capture)
(defcmd "a"           'helm-projectile-find-other-file)
(defcmd "cd"          'ex:cd)
(defcmd "tcd"         'ex:tmux-chdir)
(defcmd "t[mux]"      'ex:tmux-send)
(defcmd "r[ege]x"     'regex-tool)
(defcmd "en[ew]"      'ex:create-file)
(defcmd "l[ast]"      'popwin:popup-last-buffer)
(defcmd "run"         'ex:run-code)
(defcmd "build"       'ex:build)
(defcmd "k[ill]"      'kill-this-buffer)      ; Kill current buffer
(defcmd "k[ill]all"   'ex:kill-buffers)       ; Kill all buffers (bang = project buffers only)
(defcmd "k[ill]persp" 'my:kill-persp)         ; Kill current perspective
(defcmd "k[ill]o"     'my:kill-other-buffers) ; Kill current project buffers
(defcmd "sq[uint]"    'ex:narrow-indirect)    ; Narrow buffer to selection
(defcmd "ren[ame]"    'ex:rename-this-file)   ; Rename file . Bang: Delete old one

(after git-gutter+
       (defcmd "gstage"   'git-gutter+-stage-hunks)
       (defcmd "grevert"  'git-gutter+-revert-hunks)
       (defcmd "gdiff"    'git-gutter+-show-hunk))
