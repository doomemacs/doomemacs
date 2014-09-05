(provide 'init-python)

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (progn
    (use-package pyenv
      :init
      (progn
        (setq pyenv-show-active-python-in-modeline nil)

        (global-pyenv-mode)
        (add-hook 'python-mode-hook 'pyenv-use-corresponding)))

    (use-package jedi
      :init
      (if (not (file-directory-p "~/.emacs.d/.python-environments/default/"))
          (jedi:install-server))
	  (add-hook 'python-mode-hook 'jedi:ac-setup))

    ;; Let autopair work with triple-quotes
    (setq autopair-handle-action-fns
          (list #'autopair-default-handle-action
                #'autopair-python-triple-quote-action))

	;;; Keybindings
    (run-code-with "python" python-mode-map)
    ;; (nmap python-mode-map (kbd ",r") 'python-shell-send-buffer)
    ;; (vmap python-mode-map (kbd ",r") 'python-shell-send-region)

    ;; Don't remap backspace. Leave it to autopair, please.
    (define-key python-mode-map [backspace] nil)

    (use-package nose :commands (nose-mode)
      :config
      (setq nose-mode-map (make-sparse-keymap))
      (nmap nose-mode-map
            ",tr" 'nosetests-again
            ",ta" 'nosetests-all
            ",ts" 'nosetests-one
            ",tv" 'nosetests-module
            ",tA" 'nosetests-pdb-all
            ",tO" 'nosetests-pdb-one
            ",tV" 'nosetests-pdb-module)
      :init
      (associate-mode "/test_.+\\.py\\'" nose-mode)
      ;; (add-to-list 'auto-minor-mode-alist '("/test_.+\\.py\\'" . nose-mode)))
    )))
