;;; lang/clojure/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +clojure/repl ()
  "Open a Cider REPL and return the buffer."
  (interactive)
  (cider-jack-in)
  (current-buffer))
