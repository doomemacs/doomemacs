(require-package 'key-chord)
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.5)

;; Global keymaps ;;;;;;;;;;;;;;;

(gmap (kbd "C-x C-p") 'package-list-packages)
(gmap (kbd "M-x") 'smex)
(gmap (kbd "M-X") 'smex-major-mode-commands)

(if (is-osx) (progn
    (gmap (kbd "s-+") 'text-scale-increase)
    (gmap (kbd "s--") 'text-scale-decrease)

    (map (kbd "C-c o") 'send-to-finder)
    (map (kbd "C-c u") 'send-to-transmit)
    (map (kbd "C-c l") 'send-to-launchbar)
    (map (kbd "C-c L") 'send-dir-to-launchbar)
    (emap 'normal (kbd "C-c e") 'eval-buffer)
    (emap 'visual (kbd "C-c e") 'eval-region)
))

(map (kbd "C-c t") (lambda() (interactive) (eshell t)))
(map (kbd "C-c g") 'magit-status)
(map (kbd "<C-tab>") 'evil-numbers/inc-at-pt)
(map (kbd "<S-C-tab>") 'evil-numbers/dec-at-pt)

(map (kbd "s-o") 'ido-find-file)
(map (kbd "s-p") 'projectile-switch-project)
(map (kbd "s-f") 'projectile-find-file)
(map (kbd "s-F") 'projectile-ag)
(map (kbd "s-R") 'projectile-recentf)

(define-key evil-ex-completion-map (kbd "C-r") #'evil-ex-paste-from-register)


;; Local keymaps ;;;;;;;;;;;;;;;;

(evil-leader/set-leader ",")
(evil-leader/set-key
    "e" 'my-conf-edit
    "E" 'my-conf-find
    "/" 'imenu
    "\\" 'toggle-speedbar
    ";" 'helm-imenu
    "," 'ido-switch-buffer
    "=" 'align-regexp)

(nmap ";" 'exil-ex)

;; Easy escape from insert mode
(ichmap "jj" 'evil-normal-state)

;; Moving rows rather than lines (in case of wrapping)
(nmap "j" 'evil-next-visual-line)
(nmap "k" 'evil-previous-visual-line)

;; Commenting lines
(nmap "gcc" 'evilnc-comment-or-uncomment-lines)
(vmap "gc" 'evilnc-comment-or-uncomment-lines)

;; Enable TAB to do matchit (won't work in visual)
(evil-define-key 'normal evil-matchit-mode-map (kbd "TAB") 'evilmi-jump-items)

;; Delete without yanking
(evil-define-operator evil-destroy (beg end type register yank-handler)
    (evil-delete beg end type ?_ yank-handler))
(nmap "X" 'evil-destroy)
(nmap "Y" 'copy-to-end-of-line)     ; nnoremap Y y$
(nmap "zz" 'kill-this-buffer)       ; Close buffer

;; vnoremap < <gv
;; vnoremap > >gv
(vmap (kbd "<")
    (lambda ()
      (interactive)
      (evil-shift-left (region-beginning) (region-end))
      (evil-normal-state)
      (evil-visual-restore)))
(vmap (kbd ">")
    (lambda ()
      (interactive)
      (evil-shift-right (region-beginning) (region-end))
      (evil-normal-state)
      (evil-visual-restore)))

;; Sets fn-delete to be right-delete
(imap (kbd "<kp-delete>") 'evil-delete-char)

;; Buffer navigation
(nmap "[b" 'previous-buffer)
(nmap "]b" 'next-buffer)

;; winner-mode: window layout undo/redo (see init-core.el)
(nmap (kbd "C-w u") 'winner-undo)
(nmap (kbd "C-w C-r") 'winner-redo)

;; Make ESC quit all the things
(nmap [escape] 'keyboard-quit)
(vmap [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

;; Restore bash-esque C-w/C-a/C-e in insert mode and the minibuffer
(dolist (x (list minibuffer-local-map evil-insert-state-map))
        (define-key x (kbd "C-w") 'backward-kill-word)
        (define-key x (kbd "C-a") 'move-beginning-of-line)
        (define-key x (kbd "C-e") 'move-end-of-line)
        (define-key x (kbd "C-u") 'backward-kill-line))

;; Auto-completion
(imap (kbd "C-SPC") 'ac-fuzzy-complete)
(imap (kbd "C-S-SPC") 'ac-quick-help)

;; see elisp/rotate-text.el
(nmap (kbd "RET") 'rotate-word-at-point)
(vmap (kbd "RET") 'rotate-region)


;;;; Ex Commands ;;;;;;;;;;;;;;;;

(cmap "e[dit]" 'ido-find-file)

(provide 'core-keymaps)
