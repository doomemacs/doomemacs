;;; tools/term/autoload.el

;;;###autoload
(defun +term ()
  (interactive)
  (call-interactively 'multi-term))

;;;###autoload
(defun +term/popup ()
  (interactive)
  (require 'multi-term)
  (let* ((buffer (multi-term-get-buffer current-prefix-arg))
         (window (doom-popup-buffer buffer :popup t :align t :size 25 :select t :autokill t :noesc t)))
    (select-window window)
    (setq multi-term-buffer-list (nconc multi-term-buffer-list (list buffer)))
    (multi-term-internal)))
