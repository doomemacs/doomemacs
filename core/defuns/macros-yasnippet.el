;;; macros-yasnippet.el
;; for ../core-yasnippet.el

;;;###autoload
(defmacro add-yas-minor-mode! (mode)
  "Register minor MODES in yasnippet."
  `(after! yasnippet
     (when (boundp 'yas--extra-modes)
       (add-hook ',(intern (concat (symbol-name (cadr mode)) "-hook"))
                 (lambda ()
                   (if (symbol-value ,mode)
                       (yas-activate-extra-mode ,mode)
                     (yas-deactivate-extra-mode ,mode)))))))

(provide 'macros-yasnippet)
;;; macros-yasnippet.el ends here
