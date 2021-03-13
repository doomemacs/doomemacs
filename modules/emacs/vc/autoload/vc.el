;;; emacs/vc/autoload/vc.el -*- lexical-binding: t; -*-

(defun +vc--remote-homepage ()
  (or (let ((url (browse-at-remote--remote-ref)))
        (cdr (browse-at-remote--get-url-from-remote (car url))))
      (user-error "Can't find homepage for current project")))

(defvar browse-at-remote-prefer-symbolic)
;;;###autoload
(defun +vc/browse-at-remote (&optional arg)
  "Open URL to current file (and line if selection is active) in browser.
If prefix ARG, negate the default value of `browse-at-remote-prefer-symbolic'."
  (interactive "P")
  (require 'browse-at-remote)
  (let ((browse-at-remote-prefer-symbolic
         (if arg
             (not browse-at-remote-prefer-symbolic)
           browse-at-remote-prefer-symbolic)))
    (browse-at-remote)))

;;;###autoload
(defun +vc/browse-at-remote-kill (&optional arg)
  "Copy URL to current file (and line if selection is active) to clipboard.
If prefix ARG, negate the default value of `browse-at-remote-prefer-symbolic'."
  (interactive "P")
  (require 'browse-at-remote)
  (let ((browse-at-remote-prefer-symbolic
         (if arg
             (not browse-at-remote-prefer-symbolic)
           browse-at-remote-prefer-symbolic)))
    (browse-at-remote-kill)))

;;;###autoload
(defun +vc/browse-at-remote-homepage ()
  "Open homepage for current project in browser."
  (interactive)
  (browse-url (+vc--remote-homepage)))

;;;###autoload
(defun +vc/browse-at-remote-kill-homepage ()
  "Copy homepage URL of current project to clipboard."
  (interactive)
  (let ((url (+vc--remote-homepage)))
    (kill-new url)
    (message "Copied to clipboard: %S" url)))
