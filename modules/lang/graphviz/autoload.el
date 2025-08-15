;;; lang/graphviz/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(cl-defun +graphviz-formatter (&key _buffer scratch callback &allow-other-keys)
  "Format graphviz graphs."
  (with-current-buffer scratch
    (let ((inhibit-message t)
          (message-log-max nil))
      (goto-char (point-min))
      (graphviz-dot-indent-graph))
    (funcall callback)))

;;;###autoload
(defun +graphviz/toggle-preview ()
  "Toggle `graphviz-dot-auto-preview-on-save'."
  (interactive nil 'graphviz-dot-mode)
  (if graphviz-dot-auto-preview-on-save
      (graphviz-turn-off-live-preview)
    (graphviz-turn-on-live-preview)))
