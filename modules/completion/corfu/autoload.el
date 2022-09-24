;;; completion/corfu/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +corfu-move-to-minibuffer ()
  ;; Taken from corfu's README.
  ;; TODO: extend this to other completion front-ends.
  (interactive)
  (let ((completion-extra-properties corfu--extra)
        (completion-cycle-threshold completion-cycling))
    (apply #'consult-completion-in-region completion-in-region--data)))
