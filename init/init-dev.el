(provide 'init-dev)

(use-package dash-at-point
  :commands (dash-at-point dash-at-point-with-docset)
  :if is-mac)

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

;;; Config modes
(use-package yaml-mode :defer t
  :config (add-hook 'yaml-mode-hook 'enable-tab-width-2))

(use-package json-mode
  :mode (("\\.json\\'" . json-mode)
         ("\\.jshintrc\\'" . json-mode)))

(use-package hl-todo
  :init (add-hook 'find-file-hook 'hl-todo-mode))

(use-package regex-tool
  :commands regex-tool
  :config (setq regex-tool-backend 'perl)
  :init (add-hook 'regex-tool-mode-hook 'whitespace-newline-mode))
