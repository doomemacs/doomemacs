(provide 'init-dev)

;;; Tools
(use-package dash-at-point
  :commands (dash-at-point dash-at-point-with-docset)
  :if is-mac
  :config
  (progn
    (add-to-list 'dash-at-point-mode-alist '(ruby-mode . "rb"))
    (add-to-list 'dash-at-point-mode-alist '(python-mode . "py3"))))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(use-package rotate-text
  :commands (rotate-word-at-point rotate-region))

;;;; Syntax modes
(use-package go-mode
  :mode "\\.go\\'"
  :interpreter "go"
  :init (require 'go-autocomplete))

;;; Config modes
(use-package yaml-mode :defer t
  :config (add-hook 'yaml-mode-hook 'enable-tab-width-2))

(use-package json-mode
  :mode (("\\.json\\'" . json-mode)
         ("\\.jshintrc\\'" . json-mode)))
