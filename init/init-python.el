(provide 'init-python)

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :init
  (setq python-indent-offset 4)
  :config
  (progn
    (use-package jedi)

    (unless (file-directory-p "~/.emacs.d/.python-environments/default/")
        (jedi:install-server))

    (add-hook 'python-mode-hook 'jedi:ac-setup)
    (setq python-shell-interpreter "ipython")

    ;; Dont' remap DEL please...
    (defmap python-mode-map (kbd "DEL") nil)

	;;; Keybindings
    (add-hook! 'python-mode-hook
               (setq my-switch-to-repl-func 'python-shell-switch-to-shell
                     my-send-region-to-repl-func 'python-shell-send-region
                     my-run-code-interpreter "python"))

    (use-package nose
      :commands (nose-mode)
      :init
      (progn
        ;; Reset nose keymap, we'll set new ones in my-keymaps.el
        (defvar nose-mode-map (make-sparse-keymap))
        (associate-minor-mode "/test_.+\\.py\\'" nose-mode)))))
