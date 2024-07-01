;;; editor/multiple-cursors/autoload/vanilla-mc.el -*- lexical-binding: t; -*-
;;;###if (not (modulep! :editor evil))

;;;###autoload
(defun +mc/transient-call (func)
  (require 'multiple-cursors)
  (set-transient-map +mc-transient-map t)
  (call-interactively func))

;;;###autoload
(defun +mc/transient-mark-next-like-this ()
  (interactive)
  (+mc/transient-call #'mc/mark-next-like-this))

;;;###autoload
(defun +mc/transient-unmark-next-like-this ()
  (interactive)
  (+mc/transient-call #'mc/unmark-next-like-this))

;;;###autoload
(defun +mc/transient-mark-previous-like-this ()
  (interactive)
  (+mc/transient-call #'mc/mark-previous-like-this))

;;;###autoload
(defun +mc/transient-unmark-previous-like-this ()
  (interactive)
  (+mc/transient-call #'mc/unmark-previous-like-this))
