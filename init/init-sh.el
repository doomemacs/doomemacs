(provide 'init-sh)

;; Make shell scrips executable on save. Good!
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
