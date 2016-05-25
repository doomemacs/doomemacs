;;; macros-dash.el

;;;###autoload
(defmacro def-docset! (mode docsets)
  `(add-hook! ,mode (setq-local helm-dash-docsets ',docsets)))

(provide 'macros-dash)
;;; macros-dash.el ends here
