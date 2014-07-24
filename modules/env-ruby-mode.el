
(require-packages
 '(inf-ruby
   ac-inf-ruby
   rbenv
   ))

(add-to-list 'ac-modes 'inf-ruby-mode)
(add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)
(evil-set-initial-state 'inf-ruby-mode 'insert)

(setq ruby-indent-level 4)
(setq ruby-deep-indent-paren nil)
(require 'ruby-mode-indent-fix)

;;
(provide 'env-ruby-mode)
