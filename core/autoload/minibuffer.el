;;; ../core/autoload/minibuffer.el

;;;###autoload
(defun doom-minibuffer-kill-word ()
  "Kill a word, backwards, but only if the cursor is after
`minibuffer-prompt-end', to prevent the 'Text is read-only' warning from
monopolizing the minibuffer."
  (interactive)
  (call-interactively
   (cond ((> (point) (minibuffer-prompt-end))
          'backward-kill-word)
         ((and (fboundp 'evil-ex-p) (evil-ex-p))
          'evil-ex-delete-backward-char)
         (t
          'ivy-backward-delete-char))))

;;;###autoload
(defun doom-minibuffer-kill-line ()
  "Kill the entire line, but only if the cursor is after
`minibuffer-prompt-end', to prevent the 'Text is read-only' warning from
monopolizing the minibuffer."
  (interactive)
  (when (> (point) (minibuffer-prompt-end))
    (call-interactively 'backward-kill-sentence)))

;;;###autoload
(defun doom-minibuffer-undo ()
  "Undo an edit in the minibuffer without throwing errors."
  (interactive)
  (ignore-errors (call-interactively 'undo)))
