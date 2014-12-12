(use-package re-builder
  :defer t
  :config
  (progn
    (bind 'normal reb-mode-map
          [escape]     'reb-quit
          (kbd "C-g")  'reb-quit
          [backtab]    'reb-change-syntax)

    (defun my--reb-cleanup ()
      (replace-regexp "^[ \n]*" "" nil (point-min) (point-max))
      (text-scale-set 1.5)
      (goto-char 2))
    (add-hook 'reb-mode-hook 'my--reb-cleanup)

    (after "evil"
      (evil-set-initial-state 'reb-mode 'insert)
      (evil-ex-define-cmd "re[gex]" 're-builder))))

(use-package pcre2el
  :config
  (progn
    (bind 'normal rxt-help-mode-map [escape] 'kill-buffer-and-window)

    (after "re-builder" (setq reb-re-syntax 'pcre))
    (after "popwin"
      (push '("* Regexp Explain *" :position top :height 0.35) popwin:special-display-config))))


(provide 'init-regex)
;;; init-regex.el ends here
