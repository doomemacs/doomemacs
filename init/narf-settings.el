(set-register ?. "~/.dotfiles/")
(set-register ?d "~/Dropbox/Projects/")
(set-register ?@ "~/.emacs.d/init.el")

;;;; Keymap Fixes ;;;;;;;;;;;;;;;;;;;;;;
;; Implements some helpful keymappings for emacs sub-modes
(add-hook! 'ido-setup-hook
           (bind :map (ido-completion-map ido-file-completion-map)
                 ;; "<backspace>"  'ido-delete-backward-updir
                 "C-w"          'ido-delete-backward-word-updir))

(bind :normal :map evil-command-window-mode-map [escape] 'kill-buffer-and-window)
(bind :map evil-ex-map [escape] 'narf/minibuffer-quit)

(bind :map minibuffer-local-map "\C-u" 'evil-delete-whole-line)

;; Disable the global drag-mouse map; clicking in new buffers often sends evil
;; into visual mode, which is UN...ACCEPTAABBLLLEEEE!
(global-unset-key (kbd "<drag-mouse-1>"))

;; Don't allow quitting easily.
(setq confirm-kill-emacs (lambda (prompt) (y-or-n-p ">> Gee, I dunno Brain... Are you sure?")))


(provide 'narf-settings)
;;; narf-settings.el ends here
