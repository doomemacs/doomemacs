;;; emacs/vc/autoload/vc.el -*- lexical-binding: t; -*-

(defun +vc--remote-homepage ()
  (or (let ((url (browse-at-remote--remote-ref)))
        (cdr (browse-at-remote--get-url-from-remote (car url))))
      (user-error "Can't find homepage for current project")))

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
