;;; core/autoload/minibuffer.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom/minibuffer-kill-word ()
  "Kill a word, backwards, but only if the cursor is after
`minibuffer-prompt-end', to prevent the 'Text is read-only' warning from
monopolizing the minibuffer."
  (interactive)
  (when (> (point) (minibuffer-prompt-end))
    (call-interactively #'backward-kill-word)))

;;;###autoload
(defun doom/minibuffer-kill-line ()
  "Kill the entire line, but only if the cursor is after
`minibuffer-prompt-end', to prevent the 'Text is read-only' warning from
monopolizing the minibuffer."
  (interactive)
  (when (> (point) (minibuffer-prompt-end))
    (call-interactively #'backward-kill-sentence)))

;;;###autoload
(defun doom/minibuffer-undo ()
  "Undo an edit in the minibuffer without throwing errors."
  (interactive)
  (ignore-errors (call-interactively #'undo)))
