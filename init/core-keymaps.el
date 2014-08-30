;; Global keymaps ;;;;;;;;;;;;;;;
(global-set-key (kbd "<C-escape>") 'my/open-scratch)
(global-set-key (kbd "C-c C-p")	 'package-list-packages)
(global-set-key (kbd "M-x")	 'smex)
(global-set-key (kbd "M-X")	 'smex-major-mode-commands)

(when is-mac
  ;; TODO: Open in tmux
  (nmap my/mode-map
        (kbd "C-c o")   'send-dir-to-finder
        (kbd "C-c u")   'send-to-transmit
        (kbd "C-c l")   'send-to-launchbar
        (kbd "C-c L")   'send-dir-to-launchbar
        (kbd "C-c t")   'my/tmux-chdir
        (kbd "C-c T")   (λ (my/tmux-chdir (projectile-project-root))))

  ;; Evaluating elisp
  (nmap my/mode-map (kbd "C-c x") 'eval-buffer)
  (vmap my/mode-map (kbd "C-c x") 'eval-region)

  (when window-system
    (global-set-key (kbd "s-=")	    'text-scale-increase)
    (global-set-key (kbd "s--")	    'text-scale-decrease)
    (global-set-key (kbd "s-w")     'evil-window-delete)
    (global-set-key (kbd "s-/")     'evilnc-comment-or-uncomment-lines)
    (global-set-key (kbd "s-<f12>") 'toggle-frame-fullscreen)

    (global-set-key (kbd "C-;")   'eval-expression)
    (global-set-key (kbd "s-;")   'my/tmux-run)
    (global-set-key (kbd "s-:")   'my/tmux-paste)

    ;; Faster scrolling
    (mapc (lambda(map)
            (evil-define-key map my/mode-map (kbd "s-j") "5j")
            (evil-define-key map my/mode-map (kbd "s-k") "5k"))
          '(emacs normal visual))

    (nmap my/mode-map
          ;; Leader alternatives
          (kbd "s-t") 	'projectile-find-file
          (kbd "s-F") 	'projectile-ag
          (kbd "s-p") 	'projectile-switch-project
          (kbd "s-m") 	'my/recentf-ido-find-file
          (kbd "s-M") 	'projectile-recentf
          (kbd "s-o") 	'ido-find-file
          (kbd "s-d") 	'dash-at-point

          (kbd "s-'") 	'mc/mark-next-like-this
          (kbd "s-\"") 	'mc/mark-previous-like-this
          (kbd "C-s-'") 'mc/mark-all-like-this)

    (imap my/mode-map
          ;; Textmate-esque insert-line before/after
          (kbd "<s-return>")    'evil-open-below
          (kbd "<S-s-return>")  'evil-open-above

          ;; Fix OSX text navigation shortcuts
          (kbd "<s-left>")		'move-beginning-of-line
          (kbd "<s-right>")		'move-end-of-line
          (kbd "<s-backspace>")	'backward-kill-line

          ;; Fixes delete
          (kbd "<kp-delete>")   'delete-char)

    (imap emmet-mode-keymap
          (kbd "s-e") 'emmet-expand-yas
          (kbd "s-E") 'emmet-expand-line)))

;; Local keymaps ;;;;;;;;;;;;;;;;
(nmap my/mode-map
      ",'"       'mc/mark-next-like-this
      ",\""      'mc/mark-all-like-this

      ",e"       'ido-find-file
      ",E"       'my/initfiles
      ",g"       'git-gutter:stage-hunk
      ",G"       'git-gutter:revert-hunk
      ",m"       'my/recentf-ido-find-file	    ; recent GLOBAL files
      ",M"       'projectile-recentf				; recent PROJECT files
      ",p"       'projectile-switch-project
      ",\\"      'neotree-show
      ",|"       'neotree-hide
      ",;"       'helm-imenu
      ",:"       'my/ido-goto-symbol
      ",,"       'ido-switch-buffer
      ",."       'projectile-find-file
      ",="       'align-regexp
      (kbd ", RET") 'org-capture)

;; Remap ; to : - SPC and shift-SPC replace ; and , - have to use
;; define-key instead of n/vmap for this one to register.
(define-key evil-normal-state-map ";" 'evil-ex)
(define-key evil-visual-state-map ";" 'evil-ex)

(nmap my/mode-map
      ;; Moving rows rather than lines (in case of wrapping)
      "j"       'evil-next-visual-line
      "k"       'evil-previous-visual-line

      "X"       'evil-destroy           ; Delete without yanking

      ;; behave like D and C; yank to end of line
      "Y"       (λ (evil-yank (point) (point-at-eol)))

      "zz"      'kill-this-buffer       ; Close buffer
      "]b"      'previous-buffer
      "[b"      'next-buffer
      "]e"      'next-error
      "[e"      'previous-error
      "]h"      'git-gutter:next-hunk
      "[h"      'git-gutter:previous-hunk

      ;; winner-mode: window layout undo/redo (see init-core.el)
      (kbd "C-w u")     'winner-undo
      (kbd "C-w C-r")   'winner-redo

      ;; Increment/decrement number under cursor
      (kbd "C--")  'evil-numbers/inc-at-pt
      (kbd "C-+")  'evil-numbers/dec-at-pt)

(vmap my/mode-map
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

(imap my/mode-map
      ;; Make DEL act like expandtab in vim
      (kbd "DEL")           'backward-delete-whitespace-to-column
      ;; Join lines from insert mode
      (kbd "<M-kp-delete>") 'evil-join

      ;; Newline magic
      (kbd "RET")           'newline-and-indent
      (kbd "M-RET")         (kbd "RET DEL")
      (kbd "<C-return>")    'indent-new-comment-line

      ;; Textmate-esque indent shift left/right
      (kbd "s-[")           (λ (evil-shift-left (point-at-bol) (point-at-eol)))
      (kbd "s-]")           (λ (evil-shift-right (point-at-bol) (point-at-eol)))
      (kbd "<backtab>")     (kbd "s-["))

(emap my/mode-map
      ;; Preserve buffer-movement in emacs mode
      "j" 'evil-next-line
      "k" 'evil-previous-line

      (kbd "C-w h") 'evil-window-left
      (kbd "C-w l") 'evil-window-right
      (kbd "C-w j") 'evil-window-down
      (kbd "C-w k") 'evil-window-up)

;; Rotate-text (see elisp/rotate-text.el)
(nmap my/mode-map "!" 'rotate-word-at-point)
(vmap my/mode-map "!" 'rotate-region)

;; Easy escape from insert mode
(ichmap "jj" 'evil-normal-state)

;; Enable TAB to do matchit
(nmap evil-matchit-mode-map (kbd "TAB") 'evilmi-jump-items)

;; Real go-to-definition for elisp
(nmap emacs-lisp-mode-map "gd"
      (λ (let ((func (function-called-at-point)))
           (if func (find-function func)))))

;;;; Ex Commands ;;;;;;;;;;;;;;;;
(evil-ex-define-cmd "retab" 'untabify) ; TODO: Implement proper retab defun
(evil-ex-define-cmd "msg" 'view-echo-area-messages)
(evil-ex-define-cmd "gtd" 'open-gtd)
(evil-ex-define-cmd "n[otes]" 'my/notes)
(evil-ex-define-cmd "tcd" (λ (my/tmux-chdir (projectile-project-root))))
(evil-ex-define-cmd "ag" 'projectile-ag)
(evil-ex-define-cmd "el" 'my/initfiles)
(evil-ex-define-cmd "ba" (lambda(bang) (interactive) (if bang (my/kill-all-buffers) (my/kill-other-buffers))))

;;;; Keymap fixes ;;;;;;;;;;;;;;;
;; Make ESC quit all the things
(mapc (lambda (map)
    (define-key map [escape] 'minibuffer-quit))
      (list minibuffer-local-map
            minibuffer-local-ns-map
            minibuffer-local-completion-map
            minibuffer-local-must-match-map
            minibuffer-local-isearch-map))
(define-key evil-emacs-state-map [escape] 'evil-exit-emacs-state)
;; Close help/compilation windows with escape
(define-key help-mode-map [escape] 'kill-buffer-and-window)
(define-key compilation-mode-map [escape] 'kill-buffer-and-window)

;; Restore bash-esque keymaps in insert mode and the minibuffer
(mapc (lambda (map)
    (define-key map (kbd "C-a") 'move-beginning-of-line)
    (define-key map (kbd "C-e") 'move-end-of-line)
    (define-key map (kbd "C-u") 'backward-kill-line))
      (list minibuffer-local-map minibuffer-local-ns-map evil-insert-state-map))
(define-key evil-insert-state-map (kbd "C-w") 'backward-kill-word)
(define-key minibuffer-local-map (kbd "C-w") 'ido-delete-backward-word-updir)

(add-hook 'ido-setup-hook '(lambda ()
    ;; take that "Text is read-only" and stick it where emacs don't shine!
    (define-key ido-completion-map (kbd "<backspace>") 'ido-delete-backward-updir)
    (define-key ido-completion-map "\C-n" 'ido-next-match)
    (define-key ido-completion-map "\C-p" 'ido-prev-match)

    ;; Auto-complete on tab/space (why is it called ido-exit-minibuffer?)
    (define-key ido-completion-map " " 'ido-exit-minibuffer)))

;;
(defun minibuffer-quit ()
  "Abort recursive edit.
        In Delete Selection mode, if the mark is active, just deactivate it;
        then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun backward-kill-line ()
  (interactive)
  (evil-delete (point-at-bol) (point)))

;; Mimic expandtab in vim
(defun backward-delete-whitespace-to-column ()
  "delete back to the previous column of whitespace, or as much whitespace as possible,
or just one char if that's not possible"
  (interactive)
  (if indent-tabs-mode
      (call-interactively 'backward-delete-char-untabify)
    (let ((movement (% (current-column) tab-width))
          (p (point)))
      (when (= movement 0) (setq movement tab-width))
      (save-match-data
        (if (string-match "\\w*\\(\\s-+\\)$" (buffer-substring-no-properties (- p movement) p))
            (backward-delete-char-untabify (- (match-end 1) (match-beginning 1)))
        (call-interactively 'backward-delete-char-untabify))))))

;;
(provide 'core-keymaps)
