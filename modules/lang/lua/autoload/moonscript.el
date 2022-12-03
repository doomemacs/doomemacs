;;; lang/lua/autoload/moonscript.el -*- lexical-binding: t; -*-
;;;###if (modulep! +moonscript)

;;;###autoload
(defun +lua-moonscript-fix-single-quotes-h ()
  "Single-quoted strings aren't treated as strings."
  ;; (modify-syntax-entry ?\" "\"" moonscript-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" moonscript-mode-syntax-table))

;;;###autoload
(defun +lua-moonscript-fontify-interpolation-h ()
  "Highlight interpolated expressions in moonscript strings."
  (font-lock-add-keywords
   nil '(("#{\\([^}]+\\)}"
          (0 font-lock-preprocessor-face t)
          (1 nil t)))))
