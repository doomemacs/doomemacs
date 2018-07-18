;;; lang/latex/+preview-pane.el -*- lexical-binding: t; -*-
;;;###if (featurep! +preview-pane)

(def-package! latex-preview-pane
  :when (featurep! +preview-pane)
  :hook (LaTeX-mode . latex-preview-pane-enable)
  :commands latex-preview-pane-mode
  :init
  (setq latex-preview-pane-multifile-mode 'auctex)
  :config
  (add-to-list 'TeX-view-program-list '("preview-pane" latex-preview-pane-mode))
  (add-to-list 'TeX-view-program-selection '(output-pdf "preview-pane"))
  (define-key! doc-view-mode-map
    (kbd "ESC") #'delete-window
    "q" #'delete-window
    "k" (λ! (quit-window) (delete-window))))
