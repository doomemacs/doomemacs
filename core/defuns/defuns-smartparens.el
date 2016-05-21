;;; defuns-smartparens.el

;;;###autoload
(defun doom/sp-insert-yasnippet (id action context)
  (forward-char -1)
  (if (sp-point-after-bol-p id action context)
      (yas-expand-from-trigger-key)
    (forward-char)))

(provide 'defuns-smartparens)
;;; defuns-smartparens.el ends here
