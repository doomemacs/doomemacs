(set-register ?. "~/.dotfiles/")
(set-register ?d "~/Dropbox/Projects/")
(set-register ?@ "~/.emacs.d/init.el")

;;;; Keymap Fixes ;;;;;;;;;;;;;;;;;;;;;;
;; Implements some helpful keymappings for emacs sub-modes
(add-hook! 'ido-setup-hook
           (bind ido-completion-map
                 (kbd "<backspace>")  'ido-delete-backward-updir
                 "\C-w"               'ido-delete-backward-word-updir))

(bind 'emacs [escape] 'my--minibuffer-quit)
(bind 'normal evil-command-window-mode-map [escape] 'kill-buffer-and-window)
;; (bind evil-ex-map [escape] 'my--minibuffer-quit)

(dolist (map (list evil-ex-search-keymap minibuffer-local-map))
  (bind map "\C-w" 'evil-delete-backward-word))

(bind minibuffer-local-map "\C-u" 'evil-delete-whole-line)

;; Redefine to get rid of that silly delete-other-windows nonsense
;; (defun keyboard-escape-quit ()
;;   (interactive)
;;   (cond ((eq last-command 'mode-exited) nil)
;;         ((region-active-p)
;;          (deactivate-mark))
;;         ((> (minibuffer-depth) 0)
;;          (abort-recursive-edit))
;;         (current-prefix-arg
;;          nil)
;;         ((> (recursion-depth) 0)
;;          (exit-recursive-edit))
;;         (buffer-quit-function
;;          (funcall buffer-quit-function))
;;         ((string-match "^ \\*" (buffer-name (current-buffer)))
;;          (bury-buffer))))

(if is-mac (global-set-key (kbd "M-q") (λ (message "Gee, I dunno Brain..."))))


(provide 'my-settings)
;;; my-settings.el ends here
