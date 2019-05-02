;;; lang/clojure/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +clojure-cider-lookup-definition (identifier)
  "A lookup handler for `cider-mode'.

This is necessary to fix `cider-find-dwim's inability to capture the full symbol
at point."
  (cider-find-dwim identifier))


;;
;;; Commands

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
