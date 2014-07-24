
(require-package 'emmet-mode)

(add-hook 'scss-mode-hook   'emmet-mode) ;; Enable Emmet's css abbreviation.
(add-hook 'web-mode-hook    'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'nxml-mode-hook   'emmet-mode) ;; XML too...

(imap (kbd "s-e") 'emmet-expand-line)

(provide 'mod-emmet)
