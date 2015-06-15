;;; macros-popwin.el

;;;###autoload
(defmacro add-popwin-rule! (&rest forms)
  "Register a rule for popwin. See `popwin:special-display-config'.

    Example:
    (add-popwin-rule! \"^\\*Flycheck.*\\*$\" :regexp t :position bottom :height 0.25 :noselect t)"
  (declare (indent defun))
  `(push '(,@forms) popwin:special-display-config))

(provide 'macros-popwin)
;;; macros-popwin.el ends here
