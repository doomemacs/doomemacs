;;; lang/terra/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +terra/open-repl ()
  "Open Terra REPL."
  (interactive)
  (terra-start-process "terra" "terra")
  (pop-to-buffer terra-process-buffer))
