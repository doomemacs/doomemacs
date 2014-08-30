(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua"
  :config
  (my/setup-run-code lua-mode-map "lua")
  :init
  (progn
    (define-minor-mode love-mode
      "Buffer local minor mode for Love2D"
      :init-value nil
      :lighter " <3"
      :keymap (make-sparse-keymap) ; defines love-mode-map
      :group lua)

    (nmap love-mode-map (kbd ",b")
      (λ (shell-command (concat "open -a love.app " (projectile-project-root)))))

    (add-hook 'lua-mode-hook 'love-mode-maybe)))

(defun love-mode-maybe()
  (let ((root (projectile-project-root)))
    (if (or (string-match "[.-]love/" root)
            (file-exists-p (concat root ".love-mode")))
        (love-mode t))))

;;
(provide 'mod-love)
