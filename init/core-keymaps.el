;; Global keymaps ;;;;;;;;;;;;;;;

(gmap (kbd "<C-escape>") 'open-scratch-buffer)
(gmap (kbd "M-x")		 'smex)
(gmap (kbd "M-X")		 'smex-major-mode-commands)
(gmap (kbd "C-c C-p")	 'package-list-packages)

(when (is-osx)
    (nmap (kbd "C-c o")   'send-dir-to-finder)
    (nmap (kbd "C-c u")   'send-to-transmit)
    (nmap (kbd "C-c l")   'send-to-launchbar)
    (nmap (kbd "C-c L")   'send-dir-to-launchbar)
    (nmap (kbd "C-c t")   (lambda() (interactive) (shell)))
	(nmap (kbd "C-s-RET") 'send-to-iterm)

    ;; Evaluating elisp
    (nmap (kbd "C-c x")   'eval-buffer)
    (vmap (kbd "C-c x")   'eval-region)

	(when window-system
	  (gmap (kbd "s-+")	  'text-scale-increase)
	  (gmap (kbd "s--")	  'text-scale-decrease)
      (gmap (kbd "s-<f12>") 'toggle-frame-fullscreen)

	  (gmap (kbd "s-/")   'evilnc-comment-or-uncomment-lines)
	  (gmap (kbd "s-w")   'kill-buffer-and-window)

	  ;; Faster scrolling
	  (nmap (kbd "s-j") "5j")
	  (nmap (kbd "s-k") "5k")

	  ;; Newlines from insert mode
	  (imap (kbd "<s-return>")		'evil-open-below)
	  (imap (kbd "<S-s-return>")	'evil-open-above)

	  ;; Fix OSX text navigation shortcuts
	  (imap (kbd "<s-left>")		'move-beginning-of-line)
	  (imap (kbd "<s-right>")		'move-end-of-line)
	  (imap (kbd "<s-backspace>")	'backward-kill-line)

	  ;; Fixes delete
	  (imap (kbd "<kp-delete>")		'delete-char)

	  ;; Leader alternatives
	  (nmap (kbd "s-f") 	'projectile-find-file)
	  (nmap (kbd "s-F") 	'projectile-ag)
	  (nmap (kbd "s-m") 	'helm-recentf)
	  (nmap (kbd "s-M") 	'projectile-recentf)
	  (nmap (kbd "s-o") 	'ido-find-file)
	  (nmap (kbd "s-O") 	'open-major-mode-conf)
	  (nmap (kbd "s-d") 	'mc/mark-next-like-this)
	  (nmap (kbd "s-D") 	'mc/mark-all-like-this)
	)
)


;; Local keymaps ;;;;;;;;;;;;;;;;

(evil-leader/set-leader ",")
(evil-leader/set-key
  "`"       'open-major-mode-conf
  "d" 		'mc/mark-next-like-this
  "D"		'mc/mark-all-like-this
  "e"       'ido-find-file
  "E"       'my-init
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
)

(nmap
  ";"       'evil-ex				; Remap ; to : - SPC and shift-SPC replace ; and ,
  ":"       'eval-expression		; Elisp command

  ;; Moving rows rather than lines (in case of wrapping)
  "j"       'evil-next-visual-line'
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

(vmap
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

(imap
  (kbd "s-j")         'evil-join
  (kbd "M-SPC")       'expand-space
  (kbd "<C-return>")  'indent-new-comment-line
  )

;; Commenting lines
(nmap "gcc" 'evilnc-comment-or-uncomment-lines)
(vmap "gc"  'evilnc-comment-or-uncomment-lines)

;; Rotate-text (see elisp/rotate-text.el)
(nmap "!" 'rotate-word-at-point)
(vmap "!" 'rotate-region)

;; Enable TAB to do matchit
(evil-define-key 'normal evil-matchit-mode-map (kbd "TAB") 'evilmi-jump-items)

;; Easy escape from insert mode
(ichmap "jj" 'evil-normal-state)

;;;; Org-Mode ;;;;;;;;;;;;;;;;;;;

(evil-define-key 'normal evil-org-mode-map
  "gh"      'outline-up-heading
  "gj"      (if (fboundp 'org-forward-same-level) ;to be backward compatible with older org version
                'org-forward-same-level
              'org-forward-heading-same-level)
  "gk"      (if (fboundp 'org-backward-same-level)
                'org-backward-same-level
              'org-backward-heading-same-level)
  "gl"      'outline-next-visible-heading
  "t"       'org-todo
  "T"       '(lambda () (interactive) (evil-org-eol-call (lambda() (org-insert-todo-heading nil))))
  "H"       'org-beginning-of-line
  "L"       'org-end-of-line
  ";t"      'org-show-todo-tree
  "o"       '(lambda () (interactive) (evil-org-eol-call 'always-insert-item))
  "O"       '(lambda () (interactive) (evil-org-eol-call 'org-insert-heading))
  "$"       'org-end-of-line
  "^"       'org-beginning-of-line
  "<"       'org-metaleft
  ">"       'org-metaright
  ";a"      'org-agenda
  "-"       'org-cycle-list-bullet
  (kbd "TAB") 'org-cycle)

;; normal & insert state shortcuts.
(mapc (lambda (state)
    (evil-define-key state evil-org-mode-map
      (kbd "M-l") 'org-metaright
      (kbd "M-h") 'org-metaleft
      (kbd "M-k") 'org-metaup
      (kbd "M-j") 'org-metadown
      (kbd "M-L") 'org-shiftmetaright
      (kbd "M-H") 'org-shiftmetaleft
      (kbd "M-K") 'org-shiftmetaup
      (kbd "M-J") 'org-shiftmetadown
      (kbd "M-o") '(lambda () (interactive)
                     (evil-org-eol-call
                      '(lambda()
                         (org-insert-heading)
                         (org-metaright))))
      (kbd "M-t") '(lambda () (interactive)
                     (evil-org-eol-call
                      '(lambda()
                         (org-insert-todo-heading nil)
                         (org-metaright))))
      ))
  '(normal insert))


;;;; Ex Commands ;;;;;;;;;;;;;;;;

(cmap "retab" 'indent-region) 			; TODO: Implement proper retab defun
(cmap "msg" 'view-echo-area-messages)


;;;; Keymap fixes ;;;;;;;;;;;;;;;

;; Make ESC quit all the things
(nmap [escape] 'keyboard-quit)
(vmap [escape] 'keyboard-quit)
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

;; Preserve buffer-movement in emacs mode
(emap (kbd "C-w h") 'evil-window-left)
(emap (kbd "C-w l") 'evil-window-right)
(emap (kbd "C-w j") 'evil-window-down)
(emap (kbd "C-w k") 'evil-window-up)
(emap (kbd "s-j") "5j")
(emap (kbd "s-k") "5k")

;;
(provide 'core-keymaps)
