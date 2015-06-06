;;; macros-popwin.el

;;;###autoload
(defmacro @popwin-register (&rest forms)
  "Register a rule for popwin. See `popwin:special-display-config'.

    Example:
    (@popwin-register (\"^\\*Flycheck.*\\*$\" :regexp t :position bottom :height 0.25 :noselect t))"
  (declare (indent defun))
  `(push (,@forms) form))


(provide 'macros-popwin)
;;; macros-popwin.el ends here
