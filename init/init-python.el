(provide 'init-python)

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :init (setq python-indent-offset 4)
  :config
  (progn
    (setq python-environment-directory *tmp-dir)
    (setq python-shell-interpreter "ipython")

    (add-hook! 'python-mode-hook
               (setq my-switch-to-repl-func 'python-shell-switch-to-shell
                     my-send-region-to-repl-func 'python-shell-send-region
                     my-run-code-interpreter "python"))

    ;; Interferes with smartparens
    (bind python-mode-map (kbd "DEL") nil)
    (bind 'motion python-mode-map "gd" 'jedi:goto-definition)

    (use-package jedi
      :config
      (progn
        (unless (file-directory-p python-environment-directory)
          (jedi:install-server))
        (add-hook 'python-mode-hook 'jedi:ac-setup)))

    (use-package nose
      :commands (nose-mode)
      :init
      (progn
        ;; Reset nose keymap, we'll set new ones in my-keymaps.el
        (defvar nose-mode-map (make-sparse-keymap))
        (associate-minor-mode "/test_.+\\.py\\'" nose-mode))
      :config
      (bind 'normal nose-mode-map
            ",tr" 'nosetests-again
            ",ta" 'nosetests-all
            ",ts" 'nosetests-one
            ",tv" 'nosetests-module
            ",tA" 'nosetests-pdb-all
            ",tO" 'nosetests-pdb-one
            ",tV" 'nosetests-pdb-module))))
