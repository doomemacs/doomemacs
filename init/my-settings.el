(set-register ?. "~/.dotfiles/")
(set-register ?d "~/Dropbox/Projects/")
(set-register ?@ "~/.emacs.d/init.el")

;;;; Keymap Fixes ;;;;;;;;;;;;;;;;;;;;;;
;; Implements some helpful keymappings for emacs sub-modes
(add-hook! 'ido-setup-hook
           (bind ido-completion-map
                 "<backspace>"  'ido-delete-backward-updir
                 "C-w"          'ido-delete-backward-word-updir))

(bind 'emacs [escape] 'my--minibuffer-quit)
(bind 'normal evil-command-window-mode-map [escape] 'kill-buffer-and-window)
;; (bind evil-ex-map [escape] 'my--minibuffer-quit)

;; (dolist (map (list evil-ex-search-keymap minibuffer-local-map ido-common-completion-map ido-completion-map))
;;   (bind map "C-w" 'evil-delete-backward-word))

(bind minibuffer-local-map "\C-u" 'evil-delete-whole-line)

(global-unset-key (kbd "<drag-mouse-1>"))

(if is-mac (global-set-key (kbd "M-q") (λ (message "Gee, I dunno Brain..."))))


(provide 'my-settings)
;;; my-settings.el ends here
