;;; macros-popups.el

;;;###autoload
(defmacro def-popup! (&rest params)
  `(push ',params shackle-rules))

(provide 'macros-popups)
;;; macros-popups.el ends here
