;;; editor/lispy/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +lispy-restore-elisp-outline-settings ()
  (when (derived-mode-p 'emacs-lisp-mode)
    (setq-local outline-regexp +emacs-lisp-outline-regexp
                outline-level #'+emacs-lisp-outline-level)))
