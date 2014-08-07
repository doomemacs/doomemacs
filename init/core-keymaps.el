;; Global keymaps ;;;;;;;;;;;;;;;

(global-set-key (kbd "<C-escape>") 'my/open-scratch)
(global-set-key (kbd "C-c C-p")	 'package-list-packages)

(when is-mac
  ;; Send current file to OSX apps
  (defun open-file-with (path &optional appName)
    (if (not (string= "" appName))
        (setq appName (concat "-a " appName ".app")))
    (shell-command (concat "open " appName " " path)))

  (defun open-with (appName)
    (interactive)
    (open-file-with (buffer-file-name) appName))

  (defun send-to-transmit ()      (interactive) (open-with "Transmit"))
  (defun send-to-launchbar ()     (interactive) (open-with "LaunchBar"))
  (defun send-dir-to-launchbar () (interactive) (open-file-with default-directory "LaunchBar"))
  (defun send-dir-to-finder ()    (interactive) (open-file-with default-directory "Finder"))

  (nmap my/mode-map
        (kbd "C-c o")   'send-dir-to-finder
        (kbd "C-c u")   'send-to-transmit
        (kbd "C-c l")   'send-to-launchbar
        (kbd "C-c L")   'send-dir-to-launchbar
        ;; TODO: Open in tmux
        (kbd "C-c t")   (lambda() (interactive) (shell))
        )

  ;; Evaluating elisp
  (nmap my/mode-map (kbd "C-c x")   'eval-buffer)
  (vmap my/mode-map (kbd "C-c x")   'eval-region)

  (when window-system
    (global-set-key (kbd "s-+")	  'text-scale-increase)
    (global-set-key (kbd "s--")	  'text-scale-decrease)
    (global-set-key (kbd "s-/")   'evilnc-comment-or-uncomment-lines)
    (global-set-key (kbd "s-w")   'kill-buffer-and-window)
    (global-set-key (kbd "s-<f12>") 'toggle-frame-fullscreen)

    (nmap my/mode-map
          ;; Faster scrolling
          (kbd "s-j") "5j"
          (kbd "s-k") "5k"

          ;; Leader alternatives
          (kbd "s-f") 	'projectile-find-file
          (kbd "s-F") 	'projectile-ag
          (kbd "s-m") 	'helm-recentf
          (kbd "s-M") 	'projectile-recentf
          (kbd "s-o") 	'ido-find-file
          (kbd "s-O") 	'open-major-mode-conf
          (kbd "s-d") 	'mc/mark-next-like-this
          (kbd "s-D") 	'mc/mark-previous-like-this
          (kbd "C-s-d") 'mc/mark-all-like-this)

    ;; Newlines from insert mode
    (imap my/mode-map
          (kbd "<s-return>")    'evil-open-below
          (kbd "<S-s-return>")  'evil-open-above

          ;; Fix OSX text navigation shortcuts
          (kbd "<s-left>")		'move-beginning-of-line
          (kbd "<s-right>")		'move-end-of-line
          (kbd "<s-backspace>")	'backward-kill-line

          ;; Fixes delete
          (kbd "<kp-delete>")   'delete-char)
    )
)


;; Local keymaps ;;;;;;;;;;;;;;;;

(evil-leader/set-leader ",")
(evil-leader/set-key
  "`"       'my/notes
  "d" 		'mc/mark-next-like-this
  "D"		'mc/mark-all-like-this
  "e"       'ido-find-file
  "E"       'my/initfiles
  "f"       'projectile-find-file
  "F"       'projectile-ag
  "m"       'helm-recentf 					; recent GLOBAL files
  "M"       'projectile-recentf				; recent PROJECT files
  "p"       'projectile-switch-project
  "/"       'evilnc-comment-or-uncomment-lines
  "\\"      'neotree-show
  "|"       'neotree-hide
  ";"       'helm-imenu
  ","       'ido-switch-buffer
  "="       'align-regexp
  "x"       'kill-other-buffers
  "X"       'kill-all-buffers
  (kbd "RET") 'org-capture
)

(nmap my/mode-map
  ";"           'evil-ex            ; Remap ; to : - SPC and shift-SPC replace ; and ,
  (kbd "C-;")   'eval-expression    ; Elisp command

  ;; Moving rows rather than lines (in case of wrapping)
  "j"       'evil-next-visual-line
  "k"       'evil-previous-visual-line

  "X"       'evil-destroy           ; Delete without yanking

  ;; copy to end of line
  "Y"       (lambda()
              (interactive)
              (evil-yank (point) (point-at-eol)))

  "zz"      'kill-this-buffer       ; Close buffer
  "]b"      'previous-buffer
  "[b"      'next-buffer

  ;; winner-mode: window layout undo/redo (see init-core.el)
  (kbd "C-w u")     'winner-undo
  (kbd "C-w C-r")   'winner-redo

  ;; Increment/decrement number under cursor
  (kbd "<C-tab>")    'evil-numbers/inc-at-pt
  (kbd "<S-C-tab>")  'evil-numbers/dec-at-pt
)

(vmap my/mode-map
  ; vnoremap < <gv
  "<"       (lambda ()
              (interactive)
              (evil-shift-left (region-beginning) (region-end))
              (evil-normal-state)
              (evil-visual-restore))
  ; vnoremap > >gv
  ">"       (lambda ()
              (interactive)
              (evil-shift-right (region-beginning) (region-end))
              (evil-normal-state)
              (evil-visual-restore))
  )

(imap my/mode-map
  (kbd "s-j")         'evil-join
  (kbd "M-SPC")       'expand-space
  (kbd "<C-return>")  'indent-new-comment-line
  )

;; Commenting lines
(nmap my/mode-map "gcc" 'evilnc-comment-or-uncomment-lines)
(vmap my/mode-map "gc"  'evilnc-comment-or-uncomment-lines)

;; Rotate-text (see elisp/rotate-text.el)
(nmap my/mode-map "!" 'rotate-word-at-point)
(vmap my/mode-map "!" 'rotate-region)

;; Enable TAB to do matchit
(nmap evil-matchit-mode-map (kbd "TAB") 'evilmi-jump-items)

;; Easy escape from insert mode
(ichmap "jj" 'evil-normal-state)

;; Preserve buffer-movement in emacs mode
(emap my/mode-map
  (kbd "C-w h") 'evil-window-left
  (kbd "C-w l") 'evil-window-right
  (kbd "C-w j") 'evil-window-down
  (kbd "C-w k") 'evil-window-up
  (kbd "s-j") "5j"
  (kbd "s-k") "5k")


;;;; Ex Commands ;;;;;;;;;;;;;;;;

(evil-ex-define-cmd "retab" 'untabify) ; TODO: Implement proper retab defun
(evil-ex-define-cmd "msg" 'view-echo-area-messages)


;;;; Keymap fixes ;;;;;;;;;;;;;;;

;; Make ESC quit all the things
(nmap my/mode-map [escape] 'keyboard-quit)
(vmap my/mode-map [escape] 'keyboard-quit)
(mapc (lambda (map)
    (define-key map [escape] 'minibuffer-quit))
      (list
        minibuffer-local-map
        minibuffer-local-ns-map
        minibuffer-local-completion-map
        minibuffer-local-must-match-map
        minibuffer-local-isearch-map))
(global-set-key [escape] 'evil-exit-emacs-state)
;; Close help window with escape
(define-key global-map [escape] 'quit-window)

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
    (define-key ido-completion-map "\C-f" 'ido-next-match)
    (define-key ido-completion-map "\C-p" 'ido-prev-match)
    (define-key ido-completion-map "\C-b" 'ido-prev-match)

    ;; Auto-complete on tab/space (why is it called ido-exit-minibuffer?)
    (define-key ido-completion-map " " 'ido-exit-minibuffer)
    ))


;;
(defun backward-kill-line ()
  (interactive)
  (evil-delete (point-at-bol) (point)))

(defun minibuffer-quit ()
  "Abort recursive edit.
        In Delete Selection mode, if the mark is active, just deactivate it;
        then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun kill-other-buffers ()
  (interactive)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer))))
  (message "All other buffers killed"))

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list))
  (message "All buffers killed"))

;;
(provide 'core-keymaps)
