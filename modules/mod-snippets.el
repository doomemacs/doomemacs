
;; Fix yasnippet keymaps so they only work in insert mode (why they
;; had to make this so complicated I don't know); must be defined
;; BEFORE we include yasnippet.
(defvar yas-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (evil-define-key 'insert map [(tab)]     'yas-expand)
    (evil-define-key 'insert map (kbd "TAB") 'yas-expand)
    (evil-define-key 'insert map "\C-c&\C-s" 'yas-insert-snippet)
    (evil-define-key 'insert map "\C-c&\C-n" 'yas-new-snippet)
    (evil-define-key 'insert map "\C-c&\C-v" 'yas-visit-snippet-file)
    map))

;; Require yasnippet *after* the minor mode map's been overwritten
(require-package 'yasnippet)

;;;#yasnippet
(associate-mode 'snippet-mode '("emacs.+/snippets/") t)
(add-hook 'prog-mode-hook
          '(lambda ()
			 (yas-minor-mode)
			 (diminish 'yas-minor-mode " @")
             ))

;;
(provide 'mod-snippets)
