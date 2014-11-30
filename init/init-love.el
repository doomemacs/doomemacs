(provide 'init-love)

(defun my/build-love ()
  (shell-command (format "open -a love.app %s" (my/project-root))))

(use-package lua-mode
  :mode "\\.lua$"
  :interpreter "lua"
  :init
  (progn
    (define-minor-mode love-mode
      "Buffer local minor mode for Love2D"
      :init-value nil
      :lighter " <3"
      :keymap (make-sparse-keymap))

    (add-hook! 'lua-mode-hook
               (setq my-run-code-interpreter "lua")
               (when (and (s-matches-p "[\\.-]love/.+\\.lua$" (buffer-file-name))
                          (f--traverse-upwards (f--exists? "main.lua" it)))
                 (love-mode t)
                 (setq my-build-func 'my/build-love)))))
