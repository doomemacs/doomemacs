
(require-package 'jedi)
(if (not (file-directory-p "~/.emacs.d/.python-environments/default/"))
	(jedi:install-server))

;; (setq jedi:complete-on-dot nil)

(add-hook 'python-mode-hook 'jedi:ac-setup)
(add-hook 'python-mode-hook
          (lambda ( )
			  ;; (evil-define-key 'insert ac-mode-map (kbd "C-SPC") 'jedi:complete)

              ;; Don't remap backspace. Leave it to autopair, please.
              (define-key python-mode-map [backspace] nil)
              (evil-define-key 'normal python-mode-map (kbd "s-r")
                (lambda() (interactive) (shell-command-on-region (point-min) (point-max) "python")))
              (evil-define-key 'visual python-mode-map (kbd "s-r")
                (lambda() (interactive) (shell-command-on-region (region-beginning) (region-end) "python")))

              ;; Let autopair work with triple-quotes
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))

;;
(provide 'env-python-mode)
