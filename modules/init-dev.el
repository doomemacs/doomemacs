(use-package dash-at-point
  :if is-mac
  :commands (dash-at-point dash-at-point-with-docset))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :config
  (progn
    (setq rainbow-delimiters-outermost-only-face-count 1)
    (set-face-attribute 'rainbow-delimiters-depth-1-face nil
                        :foreground 'unspecified
                        :inherit 'my-outermost-paren-face))
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'js2-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'scss-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'ruby-mode-hook 'rainbow-delimiters-mode)))

;;; Config modes
(use-package yaml-mode
  :defer t
  :config (add-hook 'yaml-mode-hook 'enable-tab-width-2))

(use-package emr
  :commands (emr-initialize emr-show-refactor-menu)
  :init (add-hook 'prog-mode-hook 'emr-initialize)
  :config
  (progn
    (bind 'normal "gR" 'emr-show-refactor-menu)
    (bind popup-menu-keymap [escape] 'keyboard-quit)

    (after "evil" (evil-ex-define-cmd "ref[actor]" 'emr-show-refactor-menu))))

;; todo's
(use-package hl-todo :init (add-hook 'find-file-hook 'hl-todo-mode))
(use-package helm-todo :commands my:helm-todo-search)
(evil-ex-define-cmd "todo" 'my:helm-todo)


(provide 'init-dev)
;;; init-dev.el ends here
