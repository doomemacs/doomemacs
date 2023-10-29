;;; completion/corfu/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +corfu-move-to-minibuffer ()
  ;; Taken from corfu's README.
  ;; TODO: extend this to other completion front-ends.
  (interactive)
  (let ((completion-extra-properties corfu--extra)
        (completion-cycle-threshold completion-cycling))
    (apply #'consult-completion-in-region completion-in-region--data)))

;;;###autoload
(defun +corfu-smart-sep-toggle-escape ()
  "Insert `corfu-separator' or toggle escape if it's already there."
  (interactive)
  (cond ((and (char-equal (char-before) corfu-separator)
              (char-equal (char-before (1- (point))) ?\\))
         (save-excursion (delete-char -2)))
        ((char-equal (char-before) corfu-separator)
         (save-excursion (backward-char 1)
                         (insert-char ?\\)))
        (t (call-interactively #'corfu-insert-separator))))
