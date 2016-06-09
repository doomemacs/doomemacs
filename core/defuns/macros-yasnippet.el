;;; macros-yasnippet.el

;;;###autoload
(defmacro def-yas-mode! (mode)
  "Register minor MODES in yasnippet."
  `(after! yasnippet
     (add-hook! ,mode
       (if ,mode
           (yas-activate-extra-mode ,mode)
         (yas-deactivate-extra-mode ,mode)))))

(provide 'macros-yasnippet)
;;; macros-yasnippet.el ends here
