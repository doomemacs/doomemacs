(provide 'my-keymaps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global keymaps                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c C-p")    'package-list-packages)
(global-set-key (kbd "M-x")        'smex)
(global-set-key (kbd "M-X")        'smex-major-mode-commands)

(when is-mac
  ;; TODO: Open in tmux
  (nmap my-mode-map
        (kbd "C-c o")   'send-dir-to-finder
        (kbd "C-c u")   'send-to-transmit
        (kbd "C-c l")   'send-to-launchbar
        (kbd "C-c L")   'send-dir-to-launchbar
        (kbd "C-c t")   'my:tmux-chdir
        (kbd "C-c T")   (λ (my:tmux-chdir (projectile-project-root))))

  ;; Evaluating elisp
  (nmap my-mode-map (kbd "C-c x") 'eval-buffer)
  (vmap my-mode-map (kbd "C-c x") 'eval-region)

  (when window-system
    (global-set-key (kbd "s-=")     'text-scale-increase)
    (global-set-key (kbd "s--")     'text-scale-decrease)
    (global-set-key (kbd "s-w")     'evil-window-delete)
    (global-set-key (kbd "s-/")     'evilnc-comment-or-uncomment-lines)
    (global-set-key (kbd "s-<f12>") 'toggle-frame-fullscreen)

    (global-set-key (kbd "C-;")   'eval-expression)
    (global-set-key (kbd "s-;")   'my:tmux-run)
    (global-set-key (kbd "s-:")   'my:tmux-paste)

    ;; Faster scrolling
    (mapc (lambda(map)
            (evil-define-key map my-mode-map (kbd "s-j") "5j")
            (evil-define-key map my-mode-map (kbd "s-k") "5k"))
          '(emacs normal visual))

    (nmap my-mode-map
          ;; Leader alternatives
          (kbd "s-t")   'projectile-find-file
          (kbd "s-F")   'projectile-ag
          (kbd "s-p")   'projectile-switch-project
          (kbd "s-m")   'my:ex:mru
          (kbd "s-M")   'projectile-recentf
          (kbd "s-o")   'ido-find-file
          (kbd "s-d")   'dash-at-point

          (kbd "s-'")   'mc/mark-next-like-this
          (kbd "s-\"")  'mc/mark-previous-like-this
          (kbd "C-s-'") 'mc/mark-all-like-this)

    (imap my-mode-map
          ;; Textmate-esque insert-line before/after
          (kbd "<s-return>")    'evil-open-below
          (kbd "<S-s-return>")  'evil-open-above

          ;; Fix OSX text navigation shortcuts
          (kbd "<s-left>")      'evil-first-non-blank
          (kbd "<s-right>")     'move-end-of-line
          (kbd "<s-backspace>") 'my.backward-kill-to-bol-and-indent

          ;; Fixes delete
          (kbd "<kp-delete>")   'delete-char)

    (imap emmet-mode-keymap
          (kbd "s-e") 'emmet-expand-yas
          (kbd "s-E") 'emmet-expand-line)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local keymaps                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remap ; to : - SPC and shift-SPC replace ; and , - have to use
;; define-key instead of n/vmap for this one to register.
(define-key evil-normal-state-map ";" 'evil-ex)
(define-key evil-visual-state-map ";" 'evil-ex)

(nvmap my-mode-map
      "gc" 'evil-ace-jump-char-mode
      "gw" 'evil-ace-jump-word-mode        ; overwrites evil-fill
      "gl" 'evil-ace-jump-line-mode)

(nmap my-mode-map
      ;; Leader maps
      ",'"       'mc/mark-next-like-this
      ",\""      'mc/mark-all-like-this

      ",e"       'ido-find-file
      ",E"       'my:ex:init-files
      ",m"       'my:ex:mru                ; recent GLOBAL files
      ",M"       'projectile-recentf       ; recent PROJECT files
      ",p"       'projectile-switch-project
      ",\\"      'neotree-show
      ",|"       'neotree-hide
      ",;"       'helm-imenu
      ",:"       'my:ido-goto-symbol
      ",,"       'ido-switch-buffer
      ",."       'projectile-find-file
      ",="       'align-regexp

      ;; Moving rows rather than lines (in case of wrapping)
      "j"        'evil-next-visual-line
      "k"        'evil-previous-visual-line

      "X"        'evil-destroy           ; Delete without yanking

      ;; behave  like D and C; yank to end of line
      "Y"        (λ (evil-yank (point) (point-at-eol)))

      "zz"       'kill-this-buffer       ; Close buffer
      "]b"       'next-buffer
      "[b"       'previous-buffer
      "]e"       'next-error
      "[e"       'previous-error
      "]h"       'git-gutter:next-hunk
      "[h"       'git-gutter:previous-hunk

      ;; For quickly capturing notes and todos
      (kbd ", RET")     'org-capture

      ;; winner-mode: window layout undo/redo (see init-core.el)
      (kbd "C-w u")     'winner-undo
      (kbd "C-w C-r")   'winner-redo

      ;; Increment/decrement number under cursor
      (kbd "C--")  'evil-numbers/inc-at-pt
      (kbd "C-+")  'evil-numbers/dec-at-pt)

(vmap my-mode-map
      ;; vnoremap < <gv
      "<"      (λ (evil-shift-left (region-beginning) (region-end))
                  (evil-normal-state)
                  (evil-visual-restore))
      ;; vnoremap > >gv
      ">"      (λ (evil-shift-right (region-beginning) (region-end))
                  (evil-normal-state)
                  (evil-visual-restore))

      "+"      'er/expand-region
      "_"      'er/contract-region)


(imap my-mode-map
      ;; Join lines from insert mode
      (kbd "<M-kp-delete>") 'evil-join

      ;; Newline magic
      (kbd "<return>")      'my.newline-and-indent
      (kbd "<C-return>")    'evil-ret-and-indent
      (kbd "<M-return>")    (kbd "<return> DEL")      ; newline and dedent

      ;; Textmate-esque indent shift left/right
      (kbd "s-[")           (kbd "C-o m l C-o I DEL C-o ` l")
      (kbd "s-]")           (λ (evil-shift-right (point-at-bol) (point-at-eol)))
      (kbd "<backtab>")     (kbd "s-["))

(emap my-mode-map
      ;; Preserve buffer-movement in emacs mode
      "j" 'evil-next-line
      "k" 'evil-previous-line
      ";" 'linum-mode

      "o" 'send-dir-to-finder
      "u" 'send-to-transmit
      "l" 'send-to-launchbar
      "L" 'send-dir-to-launchbar
      "t" 'my:tmux-chdir
      "T" (λ (my:tmux-chdir (projectile-project-root)))

      (kbd "C-w h") 'evil-window-left
      (kbd "C-w l") 'evil-window-right
      (kbd "C-w j") 'evil-window-down
      (kbd "C-w k") 'evil-window-up)

;; Rotate-text (see elisp/rotate-text.el)
(nmap my-mode-map "!" 'rotate-word-at-point)
(vmap my-mode-map "!" 'rotate-region)

;; Easy escape from insert mode
(ichmap "jj" 'evil-normal-state)

;; Enable TAB to do matchit
(nmap evil-matchit-mode-map (kbd "TAB") 'evilmi-jump-items)

;; Real go-to-definition for elisp
(nmap emacs-lisp-mode-map "gd"
      (λ (let ((func (function-called-at-point)))
           (if func (find-function func)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ex Commands                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-ex-define-cmd "msg"         'my:ex:msg-buffer)
(evil-ex-define-cmd "recompile"   'my:ex:byte-compile-all)
(evil-ex-define-cmd "n[otes]"     'my:ex:notes)
(evil-ex-define-cmd "ini"         'my:ex:init-files)
(evil-ex-define-cmd "snip[pets]"  'my:ex:snippets)
(evil-ex-define-cmd "mru"         'my:ex:mru)

(evil-ex-define-cmd "retab"       'untabify) ; TODO: Implement proper retab defun
(evil-ex-define-cmd "ag"          'my:ex:ag-search)
(evil-ex-define-cmd "agr"         'my:ex:ag-regex-search)
(evil-ex-define-cmd "x"           'my:ex:scratch-buffer)

(evil-ex-define-cmd "bx"          'my:ex:kill-buffers)
(evil-ex-define-cmd "tcd"         'my:ex:tmux-chdir)
(evil-ex-define-cmd "tmux"        'my:ex:tmux-send)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap fixes                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Restores "dumb" indentation to the tab key. This rustles a lot of
;; peoples' jimmies, apparently, but it's how I like it.
(imap my-mode-map (kbd "<tab>") 'my.dumb-indent)
;; Except for lisp
(imap lisp-mode-map (kbd "<tab>") 'indent-for-tab-command)
(imap emacs-lisp-mode-map (kbd "<tab>") 'indent-for-tab-command)

;; Highjacks the backspace and space to:
;;   a) expand spaces between delimiters intelligently: (|) -> ( | )
;;   b) the reverse of A: ( | ) -> (|)
;;   c) And allow backspace to delete indentation blocks intelligently
(define-key evil-insert-state-map
  [remap autopair-backspace] 'my.deflate-space-maybe)
(define-key evil-insert-state-map
  (kbd "SPC") 'my.inflate-space-maybe)

;; Make ESC quit all the things
(mapc (lambda (map)
    (define-key map [escape] 'my.minibuffer-quit))
      (list minibuffer-local-map
            minibuffer-local-ns-map
            minibuffer-local-completion-map
            minibuffer-local-must-match-map
            minibuffer-local-isearch-map))
(define-key evil-emacs-state-map [escape] 'evil-exit-emacs-state)
;; Close help/compilation windows with escape
(define-key help-mode-map [escape] 'kill-buffer-and-window)
(define-key compilation-mode-map [escape] 'kill-buffer-and-window)

;; Restore bash-esque keymaps in insert mode
(imap my-mode-map
      (kbd "C-a") 'evil-move-beginning-of-line
      (kbd "C-e") 'evil-move-end-of-line
      (kbd "C-u") 'my.backward-kill-to-bol-and-indent)

;; And the minibuffer
(mapc (lambda (map)
    (define-key map (kbd "C-a") 'move-beginning-of-line)
    (define-key map (kbd "C-e") 'move-end-of-line)
    (define-key map (kbd "C-u") 'my.backward-kill-to-bol))
      (list minibuffer-local-map minibuffer-local-ns-map))
(define-key evil-insert-state-map (kbd "C-w") 'backward-kill-word)
(define-key minibuffer-local-map (kbd "C-w") 'ido-delete-backward-word-updir)

(add-hook 'ido-setup-hook '(lambda ()
    ;; take that "Text is read-only" and stick it where emacs don't shine!
    (define-key ido-completion-map (kbd "<backspace>") 'ido-delete-backward-updir)
    (define-key ido-completion-map "\C-n" 'ido-next-match)
    (define-key ido-completion-map "\C-p" 'ido-prev-match)

    ;; Auto-complete on tab/space (why is it called ido-exit-minibuffer?)
    (define-key ido-completion-map " " 'ido-exit-minibuffer)))
