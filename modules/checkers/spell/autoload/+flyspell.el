;;; checkers/spell/autoload/+flyspell.el -*- lexical-binding: t; -*-
;;;###if (featurep! +flyspell)

;;;###autodef
(defalias 'flyspell-mode! #'flyspell-mode)

(defvar +spell--flyspell-predicate-alist nil
  "TODO")

;;;###autodef
(defun set-flyspell-predicate! (modes predicate)
  "TODO"
  (declare (indent defun))
  (dolist (mode (doom-enlist modes) +spell--flyspell-predicate-alist)
    (add-to-list '+spell--flyspell-predicate-alist (cons mode predicate))))

;;;###autoload
(defun +spell-init-flyspell-predicate-h ()
  "TODO"
  (when-let (pred (assq major-mode +spell--flyspell-predicate-alist))
    (setq-local flyspell-generic-check-word-predicate (cdr pred))))

;;;###autoload
(defun +spell-correction-at-point-p (&optional point)
  "TODO"
  (cl-loop for ov in (overlays-at (or point (point)))
           if (overlay-get ov 'flyspell-overlay)
           return t))

;;;###autoload
(defun +spell/add-word (word &optional scope)
  "Add WORD to your personal dictionary, within SCOPE.

SCOPE can be `buffer' or `session' to exclude words only from the current buffer
or session. Otherwise, the addition is permanent."
  (interactive
   (list (progn (require 'flyspell)
                (car (flyspell-get-word)))
         (cond ((equal current-prefix-arg '(16))
                'session)
               ((equal current-prefix-arg '(4))
                'buffer))))
  (require 'flyspell)
  (cond
   ((null scope)
    (ispell-send-string (concat "*" word "\n"))
    (ispell-send-string "#\n")
    (flyspell-unhighlight-at (point))
    (setq ispell-pdict-modified-p '(t)))
   ((memq scope '(buffer session))
    (ispell-send-string (concat "@" word "\n"))
    (add-to-list 'ispell-buffer-session-localwords word)
    (or ispell-buffer-local-name ; session localwords might conflict
        (setq ispell-buffer-local-name (buffer-name)))
    (flyspell-unhighlight-at (point))
    (if (null ispell-pdict-modified-p)
        (setq ispell-pdict-modified-p
              (list ispell-pdict-modified-p)))
    (if (eq replace 'buffer)
        (ispell-add-per-file-word-list word))))
  (ispell-pdict-save t))

;;;###autoload
(defun +spell/remove-word (word &optional _scope)
  "Remove WORD from your personal dictionary."
  (interactive)
  (user-error "Not supported yet with +flyspell"))

;;;###autoload
(defun +spell/next-error ()
  "Jump to next flyspell error."
  (interactive)
  (call-interactively
   (if (featurep 'evil)
       #'evil-next-flyspell-error
     #'flyspell-goto-next-error)))

;;;###autoload
(defun +spell/previous-error ()
  "Jump to previous flyspell error."
  (interactive)
  (call-interactively
   (if (featurep 'evil)
       #'evil-prev-flyspell-error
     ;; TODO Implement this
     (user-error "Not supported"))))
