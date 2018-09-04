;;; emacs/ediff/config.el -*- lexical-binding: t; -*-

;; `ediff'
(setq ediff-diff-options "-w" ; turn off whitespace checking
      ediff-split-window-function #'split-window-horizontally
      ediff-window-setup-function #'ediff-setup-windows-plain)

;; Restore window config after quitting ediff
(defvar +ediff--saved-wconf nil)

(defun +ediff|save-wconf ()
  (setq +ediff--saved-wconf (current-window-configuration)))
(defun +ediff|restore-wconf ()
  (set-window-configuration +ediff--saved-wconf))

(add-hook 'ediff-before-setup-hook #'+ediff|save-wconf)
(add-hook! :append '(ediff-quit-hook ediff-suspend-hook) #'+ediff|restore-wconf)
