;;; lang/clojure/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +clojure/repl (&optional arg)
  "Open a Cider REPL and return the buffer."
  (interactive "P")
  (cider-jack-in arg)
  (current-buffer))

;;;###autoload
(defun +clojure/cider-switch-to-repl-buffer-and-switch-ns ()
  "TODO"
  (interactive)
  (cider-switch-to-repl-buffer t))
