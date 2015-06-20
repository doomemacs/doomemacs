;;; macros-yasnippet.el
;; for ../core-yasnippet.el

;;;###autoload
(defmacro add-yas-minor-mode! (&rest modes)
  "Register minor MODES in yasnippet."
  `(after! yasnippet
     (when (boundp 'yas-extra-modes)
       ,@(mapcar (lambda (mode)
                   `(after! ,(cadr mode)
                      (if (symbol-value ,mode)
                          (yas-activate-extra-mode ,mode)
                        (setq yas-extra-modes (delq ,mode yas-extra-modes)))))
                 modes))))

(provide 'macros-yasnippet)
;;; macros-yasnippet.el ends here
