
(require-package 'jedi)

(add-hook 'python-mode-hook 'jedi:setup)
(if (not (file-directory-p "~/.emacs.d/.python-environments/default/"))
	(jedi:install-server))

;;
(provide 'env-python-mode)
